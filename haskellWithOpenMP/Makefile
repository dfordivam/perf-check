main: main.hs code.c
	ghc --make main.hs code.c omp_mm.c -optc-fopenmp -optc-std=c11 \
		-optc-g -optc-pg -optc-static -optl-lgomp -optl-L/lib64

clean:
	rm main main.o main.hi code.o *.o

openmp: omp_mm.c code_openmp.c
	gcc omp_mm.c code_openmp.c -fopenmp -o openmp -std=c11 -g -pg -static
