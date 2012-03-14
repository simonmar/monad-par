


all: mandel parfib sumeuler pleasant
mandel: 
	APP=mandel_dist ./builddist.sh
	@echo
parfib: 
	APP=parfib_dist ./builddist.sh
	@echo
sumeuler: 
	APP=sumeuler_ported ./builddist.sh
	@echo
pleasant:
	APP=pleasantly_par_dist ./builddist.sh
	@echo


clean: 
	rm -f *.o *.hi *.exe
