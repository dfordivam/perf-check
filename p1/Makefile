main: main.hs task.c
	ghc -O2 --make main.hs task.c -optc-fopenmp -optc-std=c11 \
		-optc-static -optl-lgomp -optc-O3 -optl-L/lib64

main_debug: main.hs task.c
	ghc --make main.hs task.c -optc-fopenmp -optc-std=c11 \
		-optc-g -optc-pg -optc-static -optl-lgomp -optl-L/lib64

clean:
	rm main main.o main.hi code.o *.o

openmp: omp_mm.c code_openmp.c
	gcc omp_mm.c code_openmp.c -fopenmp -o openmp -std=c11 -g -pg -static
