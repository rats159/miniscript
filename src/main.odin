package main

import "core:fmt"
import "core:flags"
import "core:os"

Options :: struct {
    path: os.Handle `args:"pos=0,required" usage:"Input file."`,
    print_tokens: bool `usage:"Whether or not to print the tokens."`,
    print_ast: bool `usage:"Whether or not to print the ast."`,
}

main :: proc(){
    options: Options

    flags.parse_or_exit(&options, os.args, .Unix)

    source, open_err := os.read_entire_file_or_err(options.path)
    
    if open_err != nil {
        fmt.println(open_err)
        os.exit(1)
    }

    tokens, tokenizer_err := tokenize(transmute(string)source);

    if tokenizer_err != nil {
        print_tokenizer_error(tokenizer_err.?)
        os.exit(1)
    }

    if options.print_tokens {
        print_tokens(tokens)
    }

    parser, parser_error := parse(tokens)
    if parser_error != nil {
        print_parser_error(parser_error.?)
        os.exit(1)
    }
    if options.print_ast {
        print_tree(parser.root)
    }
    

    runtime_error := execute(parser.root)

    if runtime_error != nil {
        print_runtime_error(runtime_error.?)
        os.exit(1)
    }
}