main: main.s
	cc -o main main.s

main.s: main.ssa
	qbe -o main.s main.ssa

main.ssa: compiler main.py
	./compiler main.py main.ssa

compiler: compiler.c
	cc -ggdb -o compiler compiler.c

one_plus_one: one_plus_one.s
	cc -o one_plus_one one_plus_one.s

one_plus_one.s: one_plus_one.ssa
	qbe -o one_plus_one.s one_plus_one.ssa
