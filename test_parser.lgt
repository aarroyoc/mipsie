:- object(test_parser, extends(lgtunit)).

test(parse_sum) :-
    parser::parse("examples/sum.s", Code, State).

:- end_object.
