package prog8.compiler

import com.github.michaelbull.result.*
import prog8.ast.Module
import prog8.ast.Program
import prog8.ast.SyntaxError
import prog8.ast.statements.Directive
import prog8.ast.statements.DirectiveArg
import prog8.code.core.IErrorReporter
import prog8.code.core.Position
import prog8.code.sanitize
import prog8.code.source.ImportFileSystem
import prog8.code.source.SourceCode
import prog8.parser.Prog8Parser
import prog8.parser.ProgBParser
import java.io.File
import java.nio.file.Path
import kotlin.io.path.Path
import kotlin.io.path.exists


class ModuleImporter(private val program: Program,
                     private val compilationTargetName: String,
                     val errors: IErrorReporter,
                     sourceDirs: List<String>,
                     libraryDirs: List<String>,
                     val cwd: Path,
                     val quiet: Boolean,
                     val nostdlib: Boolean = false) {

    private class ModuleSourceConflictException(moduleName: String, p8Path: Path, pbPath: Path): Exception(
        "conflicting module sources for '$moduleName': found both ${p8Path.normalize()} and ${pbPath.normalize()}"
    )

    private val sourcePaths: List<Path> = sourceDirs.map { Path(it).sanitize() }.toSortedSet().toList()
    private val libraryPaths: List<Path> = libraryDirs.map { Path(it).sanitize() }.toSortedSet().toList()

    fun importMainModule(filePath: Path): Result<Module, NoSuchFileException> {
        val searchIn = sourcePaths.toSortedSet()
        val normalizedFilePath = filePath.normalize()
        
        if (normalizedFilePath.exists()) {
            printCompileInfo(normalizedFilePath.toAbsolutePath())
            return Ok(importModule(ImportFileSystem.getFile(normalizedFilePath)))
        }

        for(path in searchIn) {
            val programPath = path.resolve(normalizedFilePath)
            if(programPath.exists()) {
                printCompileInfo(programPath)
                val source = ImportFileSystem.getFile(programPath)
                return Ok(importModule(source))
            }
        }
        return Err(NoSuchFileException(
            file = normalizedFilePath.toFile(),
            reason = "Searched in $searchIn"))
    }

    private fun printCompileInfo(programPath: Path) {
        if(!quiet) {
            println("Compiling program ${cwd.toAbsolutePath().relativize(programPath)}")
            println("Compiler target: $compilationTargetName")
        }
    }

    fun importImplicitLibraryModule(name: String): Module? {
        val import = Directive("%import", listOf(
                DirectiveArg(name, 42u, position = Position("~implicit-import~", 0, 0, 0))
        ), Position("~implicit-import~", 0, 0, 0))
        return executeImportDirective(import, null)
    }

    private fun importModule(src: SourceCode) : Module {
        // Select parser based on file extension
        val moduleAst = if(src.origin.endsWith(".pb")) {
            ProgBParser.parseModule(src)
        } else {
            Prog8Parser.parseModule(src)
        }

        // Check if module already loaded (e.g., via symlink from different name)
        val existing = program.modules.firstOrNull { it.name == moduleAst.name }
        if (existing != null)
            return existing

        program.addModule(moduleAst)

        // accept additional imports
        try {
            val lines = moduleAst.statements.toMutableList()
            lines.asSequence()
                .mapIndexed { i, it -> i to it }
                .filter { (it.second as? Directive)?.directive == "%import" }
                .forEach { executeImportDirective(it.second as Directive, moduleAst) }
            moduleAst.statements.clear()
            moduleAst.statements.addAll(lines)
            return moduleAst
        } catch (x: Exception) {
            // in case of error, make sure the module we're importing is no longer in the Ast
            program.removeModule(moduleAst)
            throw x
        }
    }

    private fun executeImportDirective(import: Directive, importingModule: Module?): Module? {
        if(import.directive!="%import" || import.args.size!=1)
            throw SyntaxError("invalid import directive", import.position)
        val moduleName = import.args[0].string!!
        // Check for self-import (both .p8 and .pb extensions)
        if("$moduleName.p8" == import.position.file || "$moduleName.pb" == import.position.file)
            throw SyntaxError("cannot import self", import.position)

        val existing = program.modules.singleOrNull { it.name.equals(moduleName, ignoreCase = true) }
        if (existing!=null) {
            if(existing.name != moduleName) {
                errors.err("module import name '$moduleName' differs in case only from already known name '${existing.name}'", import.position)
                return null
            }
            return existing
        }

        // try internal library first (unless --nostdlib is active)
        val importedModule =
            if(!nostdlib) {
                val moduleResourceSrc = getModuleFromResource("$moduleName.p8", compilationTargetName)
                moduleResourceSrc.fold(
                    success = { importModule(it) },
                    failure = { getModuleFromFilesystem(moduleName, importingModule, import.position) }
                )
            } else {
                // skip internal libraries, go directly to filesystem
                getModuleFromFilesystem(moduleName, importingModule, import.position)
            }

        if(importedModule != null)
            removeDirectivesFromImportedModule(importedModule)
        return importedModule
    }

    private fun getModuleFromFilesystem(moduleName: String, importingModule: Module?, errorPosition: Position): Module? {
        val moduleSrc = getModuleFromFile(moduleName, importingModule)
        return moduleSrc.fold(
            success = { importModule(it) },
            failure = { failure ->
                when(failure) {
                    is ModuleSourceConflictException -> {
                        errors.err(failure.message ?: "conflicting module sources for '$moduleName'", errorPosition)
                        null
                    }
                    else -> {
                        val searchPaths = if(nostdlib) "$sourcePaths (internal libraries disabled)" else "$sourcePaths (and internal libraries)"
                        errors.err("no module found with name $moduleName. Searched in: $searchPaths", errorPosition)
                        null
                    }
                }
            }
        )
    }

    private fun removeDirectivesFromImportedModule(importedModule: Module) {
        // Most global directives don't apply for imported modules, so remove them
        val moduleLevelDirectives = listOf("%output", "%launcher", "%zeropage", "%zpreserved", "%zpallowed", "%address", "%memtop")
        var directives = importedModule.statements.filterIsInstance<Directive>()
        importedModule.statements.removeAll(directives)
        directives = directives.filter{ it.directive !in moduleLevelDirectives }
        importedModule.statements.addAll(0, directives)
    }

    private fun getModuleFromResource(name: String, compilationTargetName: String): Result<SourceCode, NoSuchFileException> {
        val result =
            runCatching { ImportFileSystem.getResource("/prog8lib/$compilationTargetName/$name") }
            .orElse { runCatching { ImportFileSystem.getResource("/prog8lib/$name") }  }

        return result.mapError { NoSuchFileException(File(name)) }
    }

    private fun getModuleFromFile(name: String, importingModule: Module?): Result<SourceCode, Exception> {

        val normalLocations =
            if (importingModule == null) {
                sourcePaths
            } else {
                val pathFromImportingModule = (Path(importingModule.position.file).parent ?: Path("")).sanitize()
                listOf(pathFromImportingModule) + sourcePaths
            }

        fun resolveFromLocations(locations: List<Path>, isLibrary: Boolean): Result<SourceCode, Exception>? {
            for(location in locations) {
                val p8File = location.resolve("$name.p8")
                val pbFile = location.resolve("$name.pb")
                val hasP8 = p8File.exists()
                val hasPb = pbFile.exists()

                if(hasP8 && hasPb)
                    return Err(ModuleSourceConflictException(name, p8File, pbFile))

                if(hasP8)
                    return Ok(ImportFileSystem.getFile(p8File, isLibrary))

                if(hasPb)
                    return Ok(ImportFileSystem.getFile(pbFile, isLibrary))
            }
            return null
        }

        // Search in library paths first
        resolveFromLocations(libraryPaths, isLibrary = true)?.let { return it }

        // Search in normal locations
        resolveFromLocations(normalLocations, isLibrary = false)?.let { return it }

        return Err(NoSuchFileException(File(name)))
    }
}
