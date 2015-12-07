main: main.hs code.c
	ghc --make main.hs code.c

clean:
	rm main main.o main.hi code.o

openmp: omp_mm.c code_openmp.c
	gcc omp_mm.c code_openmp.c -fopenmp -o openmp -std=c11 -g -pg -static
