package main

import "core:os"

source := #load("test.ms", string)

main :: proc(){
    tokens, tokenizer_err := tokenize(source);

    if tokenizer_err != nil {
        print_tokenizer_error(tokenizer_err.?)
        os.exit(1)
    }

    print_tokens(tokens)

    parser, parser_error := parse(tokens)
    if parser_error != nil {
        print_parser_error(parser_error.?)
        os.exit(1)
    }

    print_tree(parser.root)
    
    runtime_error := execute(parser.root)

    if runtime_error != nil {
        print_runtime_error(runtime_error.?)
        os.exit(1)
    }
}