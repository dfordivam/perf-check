# This is haskell main running tasks
main: main.hs task.c
	ghc -O2 --make main.hs task.c -optc-fopenmp -optc-std=c11 \
		-optc-static -optl-lgomp -optc-O3 -optl-L/lib64

# This is the equivalent cpp main running tasks
main_c : main.cpp task.c
	g++ -O3 main.cpp task.c -o main_c

main_debug: main.hs task.c
	ghc --make main.hs task.c -optc-fopenmp -optc-std=c11 \
		-optc-g -optc-pg -optc-static -optl-lgomp -optl-L/lib64

clean:
	rm -f main main_c main.o main.hi code.o *.o

openmp: omp_mm.c code_openmp.c
	gcc omp_mm.c code_openmp.c -fopenmp -o openmp -std=c11 -g -pg -static
