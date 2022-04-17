:- use_module(library(assoc)).
:- use_module(library(format)).

:- object(machine).

   :- public(execute/3).
   execute(Code, State, EndState) :-
       State = mips_state(_, PC, _, _),
       list::nth0(PC, Code, Instruction),
       execute(Instruction, State, NewState),!,
       execute(Code, NewState, EndState).

   execute(Code, State, State) :-
       State = mips_state(_, PC, _, _),
       list::length(Code, N),
       PC >= N.

    execute(add(Rd, Rs, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R0, Rs, ValRs),
	registers::value(R0, Rt, ValRt),
	ValRd is ValRs + ValRt,
	PC is PC0 + 1,
	registers::set(R0, R, Rd, ValRd).

    execute(addi(Rt, Rs, I), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R0, Rs, ValRs),
	ValRt is ValRs + I,
	PC is PC0 + 1,
	registers::set(R0, R, Rt, ValRt).

    execute(and(Rd, Rs, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R0, Rs, ValRs),
	registers::value(R0, Rt, ValRt),
	ValRd is ValRs /\ ValRt,
	PC is PC0 + 1,
	registers::set(R0, R, Rd, ValRd).

    execute(andi(Rt, Rs, I), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R0, Rs, ValRs),
	ValRt is I /\ ValRs,
	PC is PC0 + 1,
	registers::set(R0, R, Rt, ValRt).

    % in real MIPS branch instructions use relative addresses, but most assemblers work with labels too,
    % so the end result is the same as with jumps (which use absolute addresses)
    execute(beq(Rs, Rt, Label), mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R, Rs, ValRs),
	registers::value(R, Rt, ValRt),
	(
	    ValRs = ValRt ->
	    assoc:get_assoc(Label, LS, PC)
	;   PC is PC0 +1
	).

    execute(bne(Rs, Rt, Label), mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R, Rs, ValRs),
	registers::value(R, Rt, ValRt),
	(
	    ValRs \= ValRt ->
	    assoc:get_assoc(Label, LS, PC)
	;   PC is PC0 +1
	).

    execute(blt(Rs, Rt, Label), mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R, Rs, ValRs),
	registers::value(R, Rt, ValRt),
	(
	    ValRs < ValRt ->
	    assoc:get_assoc(Label, LS, PC)
	;   PC is PC0 +1
	).

    execute(bgt(Rs, Rt, Label), mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R, Rs, ValRs),
	registers::value(R, Rt, ValRt),
	(
	    ValRs > ValRt ->
	    assoc:get_assoc(Label, LS, PC)
	;   PC is PC0 +1
	).

    execute(ble(Rs, Rt, Label), mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R, Rs, ValRs),
	registers::value(R, Rt, ValRt),
	(
	    ValRs =< ValRt ->
	    assoc:get_assoc(Label, LS, PC)
	;   PC is PC0 +1
	).

    execute(bge(Rs, Rt, Label), mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R, Rs, ValRs),
	registers::value(R, Rt, ValRt),
	(
	    ValRs >= ValRt ->
	    assoc:get_assoc(Label, LS, PC)
	;   PC is PC0 +1
	).

    execute(j(Label), mips_state(R, _, M, LS), mips_state(R, PC, M, LS)) :-
	assoc:get_assoc(Label, LS, PC).

    execute(jal(Label), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	assoc:get_assoc(Label, LS, PC),
	NewPC is PC0 + 1,
	registers::set(R0, R, '$ra', NewPC).

    execute(jr(Rs), mips_state(R, _, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R, Rs, PC).

    execute(move(Rd, Rs), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R0, Rs, Val),
	registers::set(R0, R, Rd, Val),
	PC is PC0 + 1.

    execute(nor(Rd, Rs, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R0, Rs, ValRs),
	registers::value(R0, Rt, ValRt),
	ValRd is \ (ValRs \/ ValRt),
	registers::set(R0, R, Rd, ValRd),
	PC is PC0 + 1.

    execute(or(Rd, Rs, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R0, Rs, ValRs),
	registers::value(R0, Rt, ValRt),
	ValRd is ValRs \/ ValRt,
	registers::set(R0, R, Rd, ValRd),
	PC is PC0 + 1.

    execute(ori(Rt, Rs, I), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R0, Rs, ValRs),
	ValRt is I \/ ValRs,
	PC is PC0 + 1,
	registers::set(R0, R, Rt, ValRt).

    execute(li(Rd, I), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::set(R0, R, Rd, I),
	PC is PC0 + 1.

    execute(la(Rd, Label, Base, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	get_address(R0, LS, Label, Base, Rt, Addr),
	registers::set(R0, R, Rd, Addr),
	PC is PC0 + 1.

    execute(lb(Rd, Label, Base, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	get_address(R0, LS, Label, Base, Rt, Addr),
	list::nth0(Addr, M, Val),
	registers::set(R0, R, Rd, Val),
	PC is PC0 + 1.

    execute(lw(Rd, Label, Base, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	get_address(R0, LS, Label, Base, Rt, Addr),
	list::nth0(Addr, M, B3),
	Addr1 is Addr + 1, list::nth0(Addr1, M, B2),
	Addr2 is Addr + 2, list::nth0(Addr2, M, B1),
	Addr3 is Addr + 3, list::nth0(Addr3, M, B0),
	int32(Val, [B3, B2, B1, B0]),
	registers::set(R0, R, Rd, Val),
	PC is PC0 + 1.

    execute(div(Rd, Rs), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R0, Rd, ValRd),
	registers::value(R0, Rs, ValRs),
	Lo is ValRd / ValRs,
	Hi is ValRd rem ValRs,
	registers::set(R0, R1, '$__lo', Lo),
	registers::set(R1, R, '$__hi', Hi),
	PC is PC0 + 1.

    execute(mult(Rd, Rs), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R0, Rd, ValRd),
	registers::value(R0, Rs, ValRs),
	Val is ValRd * ValRs,
	Lo is Val / (2^32),
	Hi is Val >> 32,
	registers::set(R0, R1, '$__lo', Lo),
	registers::set(R1, R, '$__hi', Hi),
	PC is PC0 +1.

    execute(mfhi(Rd), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R0, '$__hi', ValRd),
	registers::set(R0, R, Rd, ValRd),
	PC is PC0 + 1.

    execute(mflo(Rd), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R0, '$__lo', ValRd),
	registers::set(R0, R, Rd, ValRd),
	PC is PC0 + 1.

    execute(sb(Rd, Label, Base, Rt), mips_state(R, PC0, M0, LS), mips_state(R, PC, M, LS)) :-
	get_address(R, LS, Label, Base, Rt, Addr),
	registers::value(R, Rd, Val),
	memory_set(M0, M, Addr, Val),
	PC is PC0 +1.

    execute(sw(Rd, Label, Base, Rt), mips_state(R, PC0, M0, LS), mips_state(R, PC, M, LS)) :-
	get_address(R, LS, Label, Base, Rt, Addr),
	registers::value(R, Rd, Val),
	int32(Val, [B3, B2, B1, B0]),
	memory_set(M0, M1, Addr, B3),
	Addr1 is Addr + 1, memory_set(M1, M2, Addr1, B2),
	Addr2 is Addr + 2, memory_set(M2, M3, Addr2, B1),
	Addr3 is Addr + 3, memory_set(M3, M, Addr3, B0),
	PC is PC0 +1.

    execute(sub(Rd, Rs, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R0, Rs, ValRs),
	registers::value(R0, Rt, ValRt),
	ValRd is ValRs - ValRt,
	registers::set(R0, R, Rd, ValRd),
	PC is PC0 + 1.

    execute(sll(Rt, Rs, I), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R0, Rs, ValRs),
	ValRt is ValRs << I,
	PC is PC0 + 1,
	registers::set(R0, R, Rt, ValRt).

    execute(srl(Rt, Rs, I), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R0, Rs, ValRs),
	ValRt is ValRs >> I,
	PC is PC0 + 1,
	registers::set(R0, R, Rt, ValRt).

    execute(slt(Rd, Rs, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R, Rs, ValRs),
	registers::value(R, Rt, ValRt),
	(
	    ValRs < ValRt ->
	    ValRd = 1
	;   ValRd = 0
	),
	PC is PC0 + 1,
	registers::set(R0, R, Rd, ValRd).

    execute(slti(Rd, Rs, I), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R, Rs, ValRs),
	(
	    ValRs < I ->
	    ValRd = 1
	;   ValRd = 0
	),
	PC is PC0 + 1,
	registers::set(R0, R, Rd, ValRd).

    % syscall - print int
    execute(syscall, mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R, '$v0', 1),
	registers::value(R, '$a0', Val),
	format:format("~d", [Val]),
	PC is PC0 + 1.

    % syscall - print asciiz
    execute(syscall, mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R, '$v0', 4),
	registers::value(R, '$a0', Addr),
	print_asciiz(Addr, M),
	PC is PC0 + 1.

    % syscall - halt
    execute(syscall, mips_state(R, PC, M, LS), mips_state(R, PC, M, LS)) :-
	registers::value(R, '$v0', 10),
	halt.

    get_address(R0, _, no_label, Base, Rt, Addr) :-
	registers::value(R0, Rt, ValRt),
	Addr is Base + ValRt.

    get_address(R0, LS, Label, Base, Rt, Addr) :-
	Label \= no_label,
	registers::value(R0, Rt, ValRt),
	assoc:get_assoc(Label, LS, ValLabel),
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
	list::nth0(Addr, M, '\x0\').
    print_asciiz(Addr, M) :-
	list::nth0(Addr, M, C),
	(atom(C) -> C = Ch; char_code(Ch, C)),
	format:format("~a", [Ch]),
	AddrNext is Addr + 1,
	print_asciiz(AddrNext, M).

:- end_object.