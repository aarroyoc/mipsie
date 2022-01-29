:- use_module(library(dif)).
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(reif)).
:- use_module(library(assoc)).

run_mips(Filename, EndMipsState) :-
    parse(Filename, MipsCode, MipsState),
    run_all(MipsCode, MipsState, EndMipsState).

parse(Filename, MipsCode, MipsState) :-
    phrase_from_file(lines(MipsCodeLines), Filename),
    maplist(no_comment_line, MipsCodeLines, MipsCodeLinesClean),
    filter_empty_lines(MipsCodeLinesClean, MipsLines),
    mips_asm(MipsLines, MipsCode, MipsState).

lines([]) --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).

line([]) --> ( "\n" | call(eos) ), !.
line([C|Cs]) --> [C], line(Cs).

eos([], []).


no_comment_line(L0, L) :-
    member(#, L0),
    phrase((seq(L), "#"), L0, _).

no_comment_line(L, L) :-
    \+ member(#, L).

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
    assoc_to_list(LS0, LS0s),
    assoc_to_list(LS1, LS1s),
    append(LS0s, LS1s, LSs),
    list_to_assoc(LSs, LS).

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
    append(M0, String, M1),
    append(M1, "\x0\", M),
    length(M0, Addr),
    put_assoc(Label, L0, Addr, L).

add_declaration_state(word(Label, Values), mips_state(R, PC, M0, L0), mips_state(R, PC, M, L)) :-
    phrase(words_bytes(Values), Bytes),
    append(M0, Bytes, M),
    length(M0, Addr),
    put_assoc(Label, L0, Addr, L).

add_declaration_state(byte(Label, Values), mips_state(R, PC, M0, L0), mips_state(R, PC, M, L)) :-
    append(M0, Values, M),
    length(M0, Addr),
    put_assoc(Label, L0, Addr, L).

add_declaration_state(space(Label, Values), mips_state(R, PC, M0, L0), mips_state(R, PC, M, L)) :-
    append(M0, Values, M),
    length(M0, Addr),
    put_assoc(Label, L0, Addr, L).

% mips_state is defined as
%    Registers
%    PC
%    Memory
%    Label-Memory
default_mips_state(mips_state(R, 0, [], LabelStore)) :-
    register_value(R, '$zero', _),
    empty_assoc(LabelStore).

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
	length(Values, N)
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
    empty_assoc(LS).
mips_label_text([Line|TextLines], N, LS) :-
    (
        phrase(((whites | []), string(Label), ":", end_line ), Line) ->
	(N1 is N, PC = N)
    ;   phrase(((whites | []), string(Label), ":", ... ), Line) -> (N1 is N + 1, PC = N); (N1 is N + 1)
    ),
    mips_label_text(TextLines, N1, LS0),
    (\+ var(PC) -> put_assoc(Label, LS0, PC, LS) ; LS0 = LS).

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

digit(D) --> [D], { char_type(D, decimal_digit) }.

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
	\+ member(+, Label),
	\+ member('(', Label)
    }.

comma -->
    ( whites | []),
    ",",
    ( whites | []).

white_char -->
    [X],
    {
        member(X, " \t")
    }.
whites --> white_char.
whites -->
       white_char,
       whites.

string(X) -->
    seq(X),
    {
	\+ member(' ', X),
	\+ member('\t', X)
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

run_all(Code, mips_state(R, PC, Memory, LabelStore), EndState) :-
    nth0(PC, Code, Instruction),
    execute(Instruction, mips_state(R, PC, Memory, LabelStore), NewState),!,
    run_all(Code, NewState, EndState).

run_all(Code, mips_state(R, PC, M, LS), mips_state(R, PC, M, LS)) :-
    length(Code, N),
    PC >= N.

run(Code, State) :-
    register_value(R0, '$zero', _),
    Code = [Instruction|_],
    execute(Instruction, mips_state(R0, 0, _, _), State).

execute(add(Rd, Rs, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, Rs, ValRs),
    register_value(R0, Rt, ValRt),
    ValRd is ValRs + ValRt,
    PC is PC0 + 1,
    registers_set(R0, R, Rd, ValRd).

execute(addi(Rt, Rs, I), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, Rs, ValRs),
    ValRt is ValRs + I,
    PC is PC0 + 1,
    registers_set(R0, R, Rt, ValRt).

execute(and(Rd, Rs, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, Rs, ValRs),
    register_value(R0, Rt, ValRt),
    ValRd is ValRs /\ ValRt,
    PC is PC0 + 1,
    registers_set(R0, R, Rd, ValRd).

execute(andi(Rt, Rs, I), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, Rs, ValRs),
    ValRt is I /\ ValRs,
    PC is PC0 + 1,
    registers_set(R0, R, Rt, ValRt).

% in real MIPS branch instructions use relative addresses, but most assemblers work with labels too,
% so the end result is the same as with jumps (which use absolute addresses)
execute(beq(Rs, Rt, Label), mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R, Rs, ValRs),
    register_value(R, Rt, ValRt),
    (
	ValRs = ValRt ->
	get_assoc(Label, LS, PC)
    ;   PC is PC0 +1
    ).

execute(bne(Rs, Rt, Label), mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R, Rs, ValRs),
    register_value(R, Rt, ValRt),
    (
	ValRs \= ValRt ->
	get_assoc(Label, LS, PC)
    ;   PC is PC0 +1
    ).

execute(blt(Rs, Rt, Label), mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R, Rs, ValRs),
    register_value(R, Rt, ValRt),
    (
	ValRs < ValRt ->
	get_assoc(Label, LS, PC)
    ;   PC is PC0 +1
    ).

execute(bgt(Rs, Rt, Label), mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R, Rs, ValRs),
    register_value(R, Rt, ValRt),
    (
	ValRs > ValRt ->
	get_assoc(Label, LS, PC)
    ;   PC is PC0 +1
    ).

execute(ble(Rs, Rt, Label), mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R, Rs, ValRs),
    register_value(R, Rt, ValRt),
    (
	ValRs =< ValRt ->
	get_assoc(Label, LS, PC)
    ;   PC is PC0 +1
    ).

execute(bge(Rs, Rt, Label), mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R, Rs, ValRs),
    register_value(R, Rt, ValRt),
    (
	ValRs >= ValRt ->
	get_assoc(Label, LS, PC)
    ;   PC is PC0 +1
    ).

execute(j(Label), mips_state(R, _, M, LS), mips_state(R, PC, M, LS)) :-
    get_assoc(Label, LS, PC).

execute(jal(Label), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    get_assoc(Label, LS, PC),
    NewPC is PC0 + 1,
    registers_set(R0, R, '$ra', NewPC).

execute(jr(Rs), mips_state(R, _, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R, Rs, PC).

execute(move(Rd, Rs), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, Rs, Val),
    registers_set(R0, R, Rd, Val),
    PC is PC0 + 1.

execute(nor(Rd, Rs, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, Rs, ValRs),
    register_value(R0, Rt, ValRt),
    ValRd is \ (ValRs \/ ValRt),
    registers_set(R0, R, Rd, ValRd),
    PC is PC0 + 1.

execute(or(Rd, Rs, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, Rs, ValRs),
    register_value(R0, Rt, ValRt),
    ValRd is ValRs \/ ValRt,
    registers_set(R0, R, Rd, ValRd),
    PC is PC0 + 1.

execute(ori(Rt, Rs, I), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, Rs, ValRs),
    ValRt is I \/ ValRs,
    PC is PC0 + 1,
    registers_set(R0, R, Rt, ValRt).

execute(li(Rd, I), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    registers_set(R0, R, Rd, I),
    PC is PC0 + 1.

execute(la(Rd, Label, Base, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    get_address(R0, LS, Label, Base, Rt, Addr),
    registers_set(R0, R, Rd, Addr),
    PC is PC0 + 1.

execute(lb(Rd, Label, Base, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    get_address(R0, LS, Label, Base, Rt, Addr),
    nth0(Addr, M, Val),
    registers_set(R0, R, Rd, Val),
    PC is PC0 + 1.

execute(lw(Rd, Label, Base, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    get_address(R0, LS, Label, Base, Rt, Addr),
    nth0(Addr, M, B3),
    Addr1 is Addr + 1, nth0(Addr1, M, B2),
    Addr2 is Addr + 2, nth0(Addr2, M, B1),
    Addr3 is Addr + 3, nth0(Addr3, M, B0),
    int32(Val, [B3, B2, B1, B0]),
    registers_set(R0, R, Rd, Val),
    PC is PC0 + 1.

execute(div(Rd, Rs), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, Rd, ValRd),
    register_value(R0, Rs, ValRs),
    Lo is ValRd / ValRs,
    Hi is ValRd rem ValRs,
    registers_set(R0, R1, '$__lo', Lo),
    registers_set(R1, R, '$__hi', Hi),
    PC is PC0 + 1.

execute(mult(Rd, Rs), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, Rd, ValRd),
    register_value(R0, Rs, ValRs),
    Val is ValRd * ValRs,
    Lo is Val / (2^32),
    Hi is Val >> 32,
    registers_set(R0, R1, '$__lo', Lo),
    registers_set(R1, R, '$__hi', Hi),
    PC is PC0 +1.

execute(mfhi(Rd), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, '$__hi', ValRd),
    registers_set(R0, R, Rd, ValRd),
    PC is PC0 + 1.

execute(mflo(Rd), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, '$__lo', ValRd),
    registers_set(R0, R, Rd, ValRd),
    PC is PC0 + 1.

execute(sb(Rd, Label, Base, Rt), mips_state(R, PC0, M0, LS), mips_state(R, PC, M, LS)) :-
    get_address(R, LS, Label, Base, Rt, Addr),
    register_value(R, Rd, Val),
    memory_set(M0, M, Addr, Val),
    PC is PC0 +1.

execute(sw(Rd, Label, Base, Rt), mips_state(R, PC0, M0, LS), mips_state(R, PC, M, LS)) :-
    get_address(R, LS, Label, Base, Rt, Addr),
    register_value(R, Rd, Val),
    int32(Val, [B3, B2, B1, B0]),
    memory_set(M0, M1, Addr, B3),
    Addr1 is Addr + 1, memory_set(M1, M2, Addr1, B2),
    Addr2 is Addr + 2, memory_set(M2, M3, Addr2, B1),
    Addr3 is Addr + 3, memory_set(M3, M, Addr3, B0),
    PC is PC0 +1.

execute(sub(Rd, Rs, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, Rs, ValRs),
    register_value(R0, Rt, ValRt),
    ValRd is ValRs - ValRt,
    registers_set(R0, R, Rd, ValRd),
    PC is PC0 + 1.

execute(sll(Rt, Rs, I), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, Rs, ValRs),
    ValRt is ValRs << I,
    PC is PC0 + 1,
    registers_set(R0, R, Rt, ValRt).

execute(srl(Rt, Rs, I), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, Rs, ValRs),
    ValRt is ValRs >> I,
    PC is PC0 + 1,
    registers_set(R0, R, Rt, ValRt).

execute(slt(Rd, Rs, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R, Rs, ValRs),
    register_value(R, Rt, ValRt),
    (
	ValRs < ValRt ->
	ValRd = 1
    ;   ValRd = 0
    ),
    PC is PC0 + 1,
    registers_set(R0, R, Rd, ValRd).

execute(slti(Rd, Rs, I), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R, Rs, ValRs),
    (
	ValRs < I ->
	ValRd = 1
    ;   ValRd = 0
    ),
    PC is PC0 + 1,
    registers_set(R0, R, Rd, ValRd).

% syscall - print int
execute(syscall, mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R, '$v0', 1),
    register_value(R, '$a0', Val),
    format("~d", [Val]),
    PC is PC0 + 1.

% syscall - print asciiz
execute(syscall, mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R, '$v0', 4),
    register_value(R, '$a0', Addr),
    print_asciiz(Addr, M),
    PC is PC0 + 1.

% syscall - halt
execute(syscall, mips_state(R, PC, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R, '$v0', 10),
    halt.

get_address(R0, _, no_label, Base, Rt, Addr) :-
    register_value(R0, Rt, ValRt),
    Addr is Base + ValRt.

get_address(R0, LS, Label, Base, Rt, Addr) :-
    Label \= no_label,
    register_value(R0, Rt, ValRt),
    get_assoc(Label, LS, ValLabel),
    Addr is ValLabel + Base + ValRt.

memory_set(M0, M, Addr, Val) :-
    memory_set_(M0, M, 0, Addr, Val).

memory_set_([], [], _, _, _).
memory_set_([M|Ms0], [M|Ms], N, Addr, Val) :-
    N \= Addr,
    NewN is N + 1,
    memory_set_(Ms0, Ms, NewN, Addr, Val).
memory_set_([_|Ms], [Val|Ms], Addr, Addr, Val).

int32(Number, [B3, B2, B1, B0]) :-
    var(Number),
    Number is (B3 << 24) + (B2 << 16) + (B1 << 8) + B0.

int32(Number, [B3, B2, B1, B0]) :-
    integer(Number),
    B0 is Number /\ 255,
    B1 is (Number >> 8) /\ 255,
    B2 is (Number >> 16) /\ 255,
    B3 is (Number >> 24) /\ 255.

print_asciiz(Addr, M) :-
    nth0(Addr, M, '\x0\').
print_asciiz(Addr, M) :-
    nth0(Addr, M, C),
    (atom(C) -> C = Ch; char_code(Ch, C)),
    format("~a", [Ch]),
    AddrNext is Addr + 1,
    print_asciiz(AddrNext, M).

registers_set(R0, R, Rd, ValRd) :-
    findall(Reg, register(Reg), Rs0),
    select(Rd, Rs0, Rs),
    maplist(register_unify(R0, R), Rs),
    register_value(R, Rd, ValRd).

register_unify(R0, R, Reg) :-
    register_value(R0, Reg, X),
    register_value(R, Reg, X).

%?- test_add.
test_add :-
    R0 = r(_,_,1,_,_,_,3,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
    execute(add('$t9', '$v1', '$a3'), mips_state(R0, 0), mips_state(R, 1)),
    R = r(_,_,1,_,_,_,3,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,4,_,_,_,_,_,_).

%?- test_sum.
test_sum :-
    run_mips("sum.s", mips_state(R, 3, _, _)),
    register_value(R, '$t0', 350),
    register_value(R, '$t1', 250).

test_hello :-
    run_mips("hello.s", _).

% registers
register(X) :- register_value(_, X, _).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$zero', 0).
register_value(r(X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$at', X).
register_value(r(_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$v0', X).
register_value(r(_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$v1', X).
register_value(r(_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$a0', X).
register_value(r(_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$a1', X).
register_value(r(_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$a2', X).
register_value(r(_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$a3', X).
register_value(r(_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$t0', X).
register_value(r(_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$t1', X).
register_value(r(_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$t2', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$t3', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$t4', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$t5', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$t6', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$t7', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$s0', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$s1', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$s2', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$s3', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_), '$s4', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_), '$s5', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_), '$s6', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_), '$s7', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_), '$t8', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_), '$t9', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_), '$k0', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_), '$k1', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_), '$gp', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_), '$sp', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_), '$fp', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_), '$ra', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_), '$__hi', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X), '$__lo', X).
