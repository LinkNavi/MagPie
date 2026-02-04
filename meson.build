project('scriptlang', 'cpp',
    version: '0.1.0',
    default_options: [
        'cpp_std=c++20',
        'warning_level=2',
    ]
)

# ---------------------------------------------------------------------------
# Dependencies
# ---------------------------------------------------------------------------
# LLVM dependency - finds LLVM via llvm-config
llvm_dep = dependency('llvm', version: '>=18.0', modules: [
    'core',
    'support',
    'irreader',
    'analysis',
    'executionengine',
    'target',
    'mcjit',
    'native',
])

# ---------------------------------------------------------------------------
# Amalgamation — combines all src/*.h into a single scriptlang.h
# This runs as a custom command during the build step.
# The output lands in build/include/scriptlang.h
# ---------------------------------------------------------------------------
amalgamator = find_program('tools/amalgamate.py')

# List your internal header files here in the order they should be combined.
# Add new headers to this list as you create them.
# NOTE: lexer.h MUST come before parser.h — Parser::parseInterpolatedString
#       instantiates a Lexer internally, so the Lexer class definition must
#       already be visible in the amalgamated output.
source_headers = files(
    'src/details/token.h',
    'src/details/ast.h',
    'src/details/types.h',
    'src/details/symbol_table.h',
    'src/details/type_checker.h',
    'src/details/semantic_analyzer.h',
    'src/details/lexer.h',
    'src/details/parser.h',
    'src/details/arena.h',
    'src/details/codegen.h',
    'src/details/jit_compiler.h',
    'src/details/hot_reload.h',
)

amalgamated_header = custom_target(
    'scriptlang.h',
    input:   source_headers,
    output:  'scriptlang.h',
    command: [amalgamator, '@INPUT@', '-o', '@OUTPUT@', '--guard', 'SCRIPTLANG_H'],
    install: true,
    install_dir: 'include',
)

# Wrap the custom_target into a proper dependency so executables can consume it.
# include_directories points to the build dir where scriptlang.h lands.
scriptlang_dep = declare_dependency(
    link_whole: [],
    sources: [amalgamated_header],
    include_directories: [include_directories('.')],
    dependencies: [llvm_dep],
)

real_dep = declare_dependency(
    link_whole: [],
    include_directories: [
        include_directories('.'),
        include_directories('src')  # Add this line
    ],
    dependencies: [llvm_dep],
)

# ---------------------------------------------------------------------------
# Test runner binary
# Discovers and runs all test_*.cpp files in the tests/ directory.
# Each test file is self-contained — just include the amalgamated header
# and write test functions.
# ---------------------------------------------------------------------------
test_sources = files(
    'tests/test_lexer.cpp',
    'tests/test_parser.cpp',
    'tests/test_semantic.cpp',
    'tests/test_codegen.cpp',
)

test_exe = executable(
    'scriptlang_tests',
    sources: test_sources + ['tests/test_main.cpp'],
    dependencies: [scriptlang_dep],
    include_directories: [include_directories('tests')],
)

test('scriptlang tests', test_exe)
executable(
    'scriptlang_jit',
    sources: ['tests/jit_runner.cpp'],
    dependencies: [scriptlang_dep],
    include_directories: [include_directories('tests')],
)

example_exe = executable(
    'scriptlang_example',
    sources: ['tools/example.cpp'],
    dependencies: [real_dep],
)

example_game_engine = executable(
    'example_game_engine',
    sources: ['tools/example_game_engine.cpp'],
    dependencies: [real_dep],
)
# ---------------------------------------------------------------------------
# Debug dump tool
# Reads a .sl source file and writes two files next to it:
#     <name>_tokens.txt   — full token stream, one per line
#     <name>_ast.txt      — indented AST tree dump
#
# Usage after building:
#     ./build/scriptlang_debug  path/to/script.sl
# ---------------------------------------------------------------------------
debug_exe = executable(
    'scriptlang_debug',
    sources: ['tools/debug_dump.cpp'],
    dependencies: [scriptlang_dep],
)

# ---------------------------------------------------------------------------
# IR Dump tool - Test LLVM IR generation
# Compiles a .sl source file and outputs LLVM IR to stdout
#
# Usage after building:
#     ./build/scriptlang_ir  path/to/script.sl
#     ./build/scriptlang_ir  path/to/script.sl > output.ll
# ---------------------------------------------------------------------------
ir_dump_exe = executable(
    'scriptlang_ir',
    sources: ['tools/ir_dump.cpp'],
    dependencies: [scriptlang_dep],
)
