:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).

:- object(parser).

    :- public(parse/3).
    parse(Filename, MipsCode, MipsState) :-
        pio:phrase_from_file(lines(MipsCodeLines), Filename),
	meta::map(no_comment_line, MipsCodeLines, MipsCodeLinesClean),
	filter_empty_lines(MipsCodeLinesClean, MipsLines),
	mips_asm(MipsLines, MipsCode, MipsState).

    (... ) --> dcgs:(... ).
    seq(X) --> dcgs:seq(X).

    lines([]) --> call(eos), !.
    lines([L|Ls]) --> line(L), lines(Ls).

    line([]) --> ( "\n" | call(eos) ), !.
    line([C|Cs]) --> [C], line(Cs).

    eos([], []).

    no_comment_line(L0, L) :-
	list::member(#, L0),
	phrase((seq(L), "#"), L0, _).

    no_comment_line(L, L) :-
	\+ list::member(#, L).

    filter_empty_lines([], []).
    filter_empty_lines([L|Ls0], Ls) :-
	(phrase(whites, L); L = ""),
	filter_empty_lines(Ls0, Ls).
    filter_empty_lines([L|Ls0], [L|Ls]) :-
	\+ (phrase(whites, L); L = ""),
	filter_empty_lines(Ls0, Ls).

    mips_asm(Lines, Code, mips_state(R, PC, M, LS)) :-
	extract_data_lines(Lines, DataLines),
	mips_data(DataLines, mips_state(R, PC, M, LS0)),
	extract_text_lines(Lines, TextLines),
	mips_label_text(TextLines, Labels),
	merge_assoc(LS0, Labels, LS),
	mips_text(TextLines, Code).

    merge_assoc(LS0, LS1, LS) :-
	assoc:assoc_to_list(LS0, LS0s),
	assoc:assoc_to_list(LS1, LS1s),
	list::append(LS0s, LS1s, LSs),
	assoc:list_to_assoc(LSs, LS).

    extract_data_lines([], []).
    extract_data_lines([L|Lines], DataLines) :-
	(
	    phrase((... , ".data", ... ), L) ->
	    extract_data_lines_end(Lines, DataLines)
	;	extract_data_lines(Lines, DataLines)
	).

    extract_data_lines_end([], []).
    extract_data_lines_end([L|Lines], DataLines) :-
	(
	    phrase((... , ".text", ... ), L) ->
	    DataLines = []
	;	(DataLines = [L|Ds], extract_data_lines_end(Lines, Ds))
	).


    extract_text_lines([], []).
    extract_text_lines([L|Lines], TextLines) :-
	(
	    phrase((... , ".text", ... ), L) ->
	    Lines = TextLines
	;	extract_text_lines(Lines, TextLines)
	).

    mips_data([], State) :- default_mips_state(State).
    mips_data([L|Ls], State) :-
	phrase(mips_data_declaration(Declaration), L),
	mips_data(Ls, State0),
	add_declaration_state(Declaration, State0, State).

    add_declaration_state(asciiz(Label, String), mips_state(R, PC, M0, L0), mips_state(R, PC, M, L)) :-
	list::append(M0, String, M1),
	list::append(M1, "\x0\", M),
	list::length(M0, Addr),
	assoc:put_assoc(Label, L0, Addr, L).

    add_declaration_state(word(Label, Values), mips_state(R, PC, M0, L0), mips_state(R, PC, M, L)) :-
	phrase(words_bytes(Values), Bytes),
	list::append(M0, Bytes, M),
	list::length(M0, Addr),
	assoc:put_assoc(Label, L0, Addr, L).

    add_declaration_state(byte(Label, Values), mips_state(R, PC, M0, L0), mips_state(R, PC, M, L)) :-
	list::append(M0, Values, M),
	list::length(M0, Addr),
	assoc:put_assoc(Label, L0, Addr, L).

    add_declaration_state(space(Label, Values), mips_state(R, PC, M0, L0), mips_state(R, PC, M, L)) :-
	list::append(M0, Values, M),
	list::length(M0, Addr),
	assoc:put_assoc(Label, L0, Addr, L).

    % mips_state is defined as
    %    Registers
    %    PC
    %    Memory
    %    Label-Memory
    default_mips_state(mips_state(R, 0, [], LabelStore)) :-
	registers::value(R, '$zero', _),
	assoc:empty_assoc(LabelStore).

    words_bytes([]) --> [].
    words_bytes([W|Words]) -->
	int32(W),
	words_bytes(Words).

    int32(Number) -->
	{
	    B0 is Number /\ 255,
	    B1 is (Number >> 8) /\ 255,
	    B2 is (Number >> 16) /\ 255,
	    B3 is (Number >> 24) /\ 255
	},
	[B3, B2, B1, B0].

    mips_data_declaration(asciiz(Label, String)) -->
	seq(Label),
	":",
	whites,
	".asciiz",
	whites,
	"\"",
	escaped_string(String),
	"\"",
	end_line.

    mips_data_declaration(word(Label, Values)) -->
	seq(Label),
	":",
	whites,
	".word",
	whites,
	values(Values),
	end_line.

    mips_data_declaration(byte(Label, Values)) -->
	seq(Label),
	":",
	whites,
	".byte",
	whites,
	values(Values),
	end_line.

    mips_data_declaration(space(Label, Values)) -->
	seq(Label),
	":",
	whites,
	".space",
	whites,
	number(N),
	end_line,
	{
	    list::length(Values, N)
	}.

    values([V]) --> number(V).
    values([V|Vs]) -->
	number(V),
	comma,
	values(Vs).


    escaped_string([]) --> [].
    escaped_string([X|Str]) -->
	[X],
	{
	    X \= (\)
	},
	escaped_string(Str).
    escaped_string(['\n'|Str]) -->
	"\\n",
	escaped_string(Str).

    mips_label_text(TextLines, LS) :-
	mips_label_text(TextLines, 0, LS).
    mips_label_text([], _, LS) :-
	assoc:empty_assoc(LS).
    mips_label_text([Line|TextLines], N, LS) :-
	(
	    phrase(((whites | []), string(Label), ":", end_line ), Line) ->
	    (N1 is N, PC = N)
	;   phrase(((whites | []), string(Label), ":", ... ), Line) -> (N1 is N + 1, PC = N); (N1 is N + 1)
	),
	mips_label_text(TextLines, N1, LS0),
	(nonvar(PC) -> assoc:put_assoc(Label, LS0, PC, LS) ; LS0 = LS).

    mips_text([], []).
    mips_text([Line|TextLines], [Instruction|Code]) :-
	phrase(mips_instruction(Instruction), Line),
	mips_text(TextLines, Code).
    mips_text([Line|TextLines], Code) :-
	phrase((... , ":", ...), Line),
	mips_text(TextLines, Code).

    instruction(add, r).
    instruction(addi, i).
    instruction(and, r).
    instruction(andi, i).
    instruction(la, i_a).
    instruction(lb, i_a).
    instruction(lw, i_a).
    instruction(nor, r).
    instruction(or, r).
    instruction(ori, i).
    instruction(sb, i_a).
    instruction(sll, i).
    instruction(slt, r).
    instruction(slti, i).
    instruction(srl, i).
    instruction(sub, r).
    instruction(sw, i_a).

    r_instruction(Instruction) -->
	{
	    instruction(Name, r),
	    atom_chars(Name, SName),
	    Instruction =.. [Name, Rd, Rs, Rt]
	},
	optional_label,
	SName,
	whites,
	register(Rd),
	comma,
	register(Rs),
	comma,
	register(Rt),
	end_line.
    i_instruction(Instruction) -->
	{
	    instruction(Name, i),
	    atom_chars(Name, SName),
	    Instruction =.. [Name, Rt, Rs, I]
	},
	optional_label,
	SName,
	whites,
	register(Rt),
	comma,
	register(Rs),
	comma,
	number(I),
	end_line.

    i_a_instruction(Instruction) -->
	{
	    instruction(Name, i_a),
	    atom_chars(Name, SName),
	    Instruction =.. [Name, Rd, Label, Base, Rt]
	},
	optional_label,
	SName,
	whites,
	register(Rd),
	comma,
	address(Label, Base, Rt),
	end_line.

    mips_instruction(Instruction) -->
	r_instruction(Instruction).
    mips_instruction(Instruction) -->
	i_instruction(Instruction).
    mips_instruction(Instruction) -->
	i_a_instruction(Instruction).

    mips_instruction(beq(Rs, Rt, Label)) -->
	optional_label,
	"beq",
	whites,
	register(Rs),
	comma,
	register(Rt),
	comma,
	string(Label),
	end_line.

    mips_instruction(bne(Rs, Rt, Label)) -->
	optional_label,
	"bne",
	whites,
	register(Rs),
	comma,
	register(Rt),
	comma,
	string(Label),
	end_line.

    mips_instruction(blt(Rs, Rt, Label)) -->
	optional_label,
	"blt",
	whites,
	register(Rs),
	comma,
	register(Rt),
	comma,
	string(Label),
	end_line.


    mips_instruction(bgt(Rs, Rt, Label)) -->
	optional_label,
	"bgt",
	whites,
	register(Rs),
	comma,
	register(Rt),
	comma,
	string(Label),
	end_line.

    mips_instruction(ble(Rs, Rt, Label)) -->
	optional_label,
	"ble",
	whites,
	register(Rs),
	comma,
	register(Rt),
	comma,
	string(Label),
	end_line.

    mips_instruction(bge(Rs, Rt, Label)) -->
	optional_label,
	"bge",
	whites,
	register(Rs),
	comma,
	register(Rt),
	comma,
	string(Label),
	end_line.

    mips_instruction(j(Label)) -->
	optional_label,
	"j",
	whites,
	string(Label),
	end_line.

    mips_instruction(jal(Label)) -->
	optional_label,
	"jal",
	whites,
	string(Label),
	end_line.

    mips_instruction(jr(Rs)) -->
	optional_label,
	"jr",
	whites,
	register(Rs),
	end_line.

    mips_instruction(li(Rd, I)) -->
	optional_label,
	"li",
	whites,
	register(Rd),
	comma,
	number(I),
	end_line.

    mips_instruction(mfhi(Rd)) -->
	optional_label,
	"mfhi",
	whites,
	register(Rd),
	end_line.

    mips_instruction(mflo(Rd)) -->
	optional_label,
	"mflo",
	whites,
	register(Rd),
	end_line.

    mips_instruction(move(Rd, Rs)) -->
	optional_label,
	"move",
	whites,
	register(Rd),
	comma,
	register(Rs),
	end_line.

    mips_instruction(mult(Rd, Rs)) -->
	optional_label,
	"mult",
	whites,
	register(Rd),
	comma,
	register(Rs),
	end_line.

    mips_instruction(div(Rd, Rs)) -->
	optional_label,
	"div",
	whites,
	register(Rd),
	comma,
	register(Rs),
	end_line.

    mips_instruction(syscall) -->
	optional_label,
	"syscall",
	end_line.

    number(D) -->
	"-",
	number_(Ds),
	{
	    number_chars(D, [-|Ds])
	}.
    number(D) -->
	number_(Ds),
	{
	    number_chars(D, Ds)
	}.

    number_([D|Ds]) --> digit(D), number_(Ds).
    number_([D]) --> digit(D).

    digit(D) --> [D], { charsio:char_type(D, decimal_digit) }.

    optional_label --> whites.
    optional_label --> ..., ":", whites.
    optional_label --> [].

    address(no_label, 0, Rt) -->
	"(",
	register(Rt),
	")".

    address(no_label, Base, Rt) -->
	number(Base),
	"(",
	register(Rt),
	")".

    address(Label, Base, '$zero') -->
	string(Label),
	whites,
	"+",
	whites,
	number(Base).

    address(Label, Base, Rt) -->
	string(Label),
	whites,
	"+",
	whites,
	number(Base),
	"(",
	register(Rt),
	")".

    address(Label, 0, '$zero') -->
	string(Label),
	{
	    \+ list::member(+, Label),
	    \+ list::member('(', Label)
	}.

    comma -->
	( whites | []),
	",",
	( whites | []).

    white_char -->
	[X],
	{
	    list::member(X, " \t")
	}.
    whites --> white_char.
    whites -->
	   white_char,
	   whites.

    string(X) -->
	seq(X),
	{
	    \+ list::member(' ', X),
	    \+ list::member('\t', X)
	}.

    end_line --> ( whites | []).

    register('$zero') --> "$zero".
    register('$zero') --> "$0".
    register('$at') --> "$at".
    register('$v0') --> "$v0".
    register('$v1') --> "$v1".
    register('$a0') --> "$a0".
    register('$a1') --> "$a1".
    register('$a2') --> "$a2".
    register('$a3') --> "$a3".
    register('$t0') --> "$t0".
    register('$t1') --> "$t1".
    register('$t2') --> "$t2".
    register('$t3') --> "$t3".
    register('$t4') --> "$t4".
    register('$t5') --> "$t5".
    register('$t6') --> "$t6".
    register('$t7') --> "$t7".
    register('$s0') --> "$s0".
    register('$s1') --> "$s1".
    register('$s2') --> "$s2".
    register('$s3') --> "$s3".
    register('$s4') --> "$s4".
    register('$s5') --> "$s5".
    register('$s6') --> "$s6".
    register('$s7') --> "$s7".
    register('$t8') --> "$t8".
    register('$t9') --> "$t9".
    register('$k0') --> "$k0".
    register('$k1') --> "$k1".
    register('$gp') --> "$gp".
    register('$sp') --> "$sp".
    register('$fp') --> "$fp".
    register('$ra') --> "$ra".
:- end_object.