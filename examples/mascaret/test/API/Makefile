CC=gcc
LD=gcc
CFLAGS=-O2
LDFLAGS=-O2
# Compilation and link options for a particular OS
ifdef SYSTEMROOT
# Windows OS
	GP = gp_masc.exe
	LDFLAGSLIB = ../../bin/mascaret.dll
else
	ifeq ($(shell uname),Linux)
# GNU/Linux OS
		GP = gp_masc
		LDFLAGSLIB = ../../bin/mascaret.so
	else
# Mac OSX
		GP = gp_mascOSX
		LDFLAGSLIB = ../../bin/mascaret.dylib
	endif
endif

.DEFAULT:
	-touch $@
all: $(GP)
main.o: ./main.c cte_GP.h TestList.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./main.c
test10_mascaret.o: ./test10_mascaret.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test10_mascaret.c
test10_sarap.o: ./test10_sarap.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test10_sarap.c
test11_rezo.o: ./test11_rezo.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test11_rezo.c
test11_sarap.o: ./test11_sarap.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test11_sarap.c
test12_mascaret_exp.o: ./test12_mascaret_exp.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test12_mascaret_exp.c
test12_mascaret_imp.o: ./test12_mascaret_imp.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test12_mascaret_imp.c
test12_rezo.o: ./test12_rezo.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test12_rezo.c
test13_mascaret.o: ./test13_mascaret.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test13_mascaret.c
test13_rezo.o: ./test13_rezo.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test13_rezo.c
test14_rezo.o: ./test14_rezo.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test14_rezo.c
test15_mascaret.o: ./test15_mascaret.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test15_mascaret.c
test15_rezo1.o: ./test15_rezo1.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test15_rezo1.c
test15_rezo2.o: ./test15_rezo2.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test15_rezo2.c
test16_mascaret1.o: ./test16_mascaret1.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test16_mascaret1.c
test16_mascaret2.o: ./test16_mascaret2.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test16_mascaret2.c
test16_sarap1.o: ./test16_sarap1.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test16_sarap1.c
test16_sarap2.o: ./test16_sarap2.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test16_sarap2.c
test17_mascaret.o: ./test17_mascaret.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test17_mascaret.c
test17_sarap.o: ./test17_sarap.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test17_sarap.c
test18_mascaret.o: ./test18_mascaret.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test18_mascaret.c
test18_sarap.o: ./test18_sarap.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test18_sarap.c
test19_mascaret.o: ./test19_mascaret.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test19_mascaret.c
test19_sarap.o: ./test19_sarap.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test19_sarap.c
test1_mascaret_exp.o: ./test1_mascaret_exp.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test1_mascaret_exp.c
test1_mascaret_imp.o: ./test1_mascaret_imp.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test1_mascaret_imp.c
test1_sarap.o: ./test1_sarap.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test1_sarap.c
test20_mascaret.o: ./test20_mascaret.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test20_mascaret.c
test21_mascaret.o: ./test21_mascaret.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test21_mascaret.c
test22_mascaret.o: ./test22_mascaret.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test22_mascaret.c
test22_sarap.o: ./test22_sarap.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test22_sarap.c
test23_mascaret.o: ./test23_mascaret.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test23_mascaret.c
test24_mascaret_exp.o: ./test24_mascaret_exp.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test24_mascaret_exp.c
test24_mascaret_imp.o: ./test24_mascaret_imp.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test24_mascaret_imp.c
test25_rezo1.o: ./test25_rezo1.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test25_rezo1.c
test25_rezo2.o: ./test25_rezo2.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test25_rezo2.c    
test26_casier.o: ./test26_casier.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test26_casier.c
test27_casier.o: ./test27_casier.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test27_casier.c
test28_mascaret.o: ./test28_mascaret.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test28_mascaret.c
test2_mascaret_exp.o: ./test2_mascaret_exp.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test2_mascaret_exp.c
test2_sarap.o: ./test2_sarap.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test2_sarap.c
test3_mascaret_exp.o: ./test3_mascaret_exp.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test3_mascaret_exp.c
test3_mascaret_imp.o: ./test3_mascaret_imp.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test3_mascaret_imp.c
test3_sarap.o: ./test3_sarap.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test3_sarap.c
test4_sarap.o: ./test4_sarap.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test4_sarap.c
test5_mascaret_exp.o: ./test5_mascaret_exp.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test5_mascaret_exp.c
test5_sarap.o: ./test5_sarap.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test5_sarap.c
test6_mascaret1.o: ./test6_mascaret1.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test6_mascaret1.c
test6_mascaret2.o: ./test6_mascaret2.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test6_mascaret2.c
test7_rezo.o: ./test7_rezo.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test7_rezo.c
test7_sarap.o: ./test7_sarap.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test7_sarap.c
test8_rezo.o: ./test8_rezo.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test8_rezo.c
test8_sarap.o: ./test8_sarap.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test8_sarap.c
test9_rezo.o: ./test9_rezo.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test9_rezo.c
test9_sarap.o: ./test9_sarap.c apimascaret.h cte_GP.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I.	./test9_sarap.c
SRC = ./test17_mascaret.c ./test20_mascaret.c ./test15_mascaret.c ./test12_rezo.c ./test23_mascaret.c ./test25_rezo1.c ./test25_rezo2.c ./test1_sarap.c ./test13_rezo.c ./test14_rezo.c ./test22_sarap.c ./test18_sarap.c ./test6_mascaret1.c ./test9_rezo.c ./test21_mascaret.c ./test13_mascaret.c ./test7_sarap.c ./test24_mascaret_exp.c ./test19_sarap.c ./test5_sarap.c ./test15_rezo1.c ./test6_mascaret2.c ./test18_mascaret.c ./test27_casier.c ./test2_mascaret_exp.c ./test10_sarap.c ./test3_sarap.c ./test16_mascaret1.c ./test2_sarap.c ./main.c ./test12_mascaret_imp.c ./test16_mascaret2.c ./test3_mascaret_exp.c ./test16_sarap1.c ./test26_casier.c ./test12_mascaret_exp.c ./test4_sarap.c ./test10_mascaret.c ./test28_mascaret.c ./test19_mascaret.c ./test24_mascaret_imp.c ./test7_rezo.c ./test17_sarap.c ./test9_sarap.c ./test11_rezo.c ./test15_rezo2.c ./test22_mascaret.c ./test1_mascaret_exp.c ./test8_rezo.c ./test1_mascaret_imp.c ./test5_mascaret_exp.c ./test8_sarap.c ./test16_sarap2.c ./test3_mascaret_imp.c ./test11_sarap.c TestList.h apimascaret.h cte_GP.h
OBJ = test17_mascaret.o test20_mascaret.o test15_mascaret.o test12_rezo.o test23_mascaret.o test25_rezo1.o test25_rezo2.o test1_sarap.o test13_rezo.o test14_rezo.o test22_sarap.o test18_sarap.o test6_mascaret1.o test9_rezo.o test21_mascaret.o test13_mascaret.o test7_sarap.o test24_mascaret_exp.o test19_sarap.o test5_sarap.o test15_rezo1.o test6_mascaret2.o test18_mascaret.o test27_casier.o test2_mascaret_exp.o test10_sarap.o test3_sarap.o test16_mascaret1.o test2_sarap.o main.o test12_mascaret_imp.o test16_mascaret2.o test3_mascaret_exp.o test16_sarap1.o test26_casier.o test12_mascaret_exp.o test4_sarap.o test10_mascaret.o test28_mascaret.o test19_mascaret.o test24_mascaret_imp.o test7_rezo.o test17_sarap.o test9_sarap.o test11_rezo.o test15_rezo2.o test22_mascaret.o test1_mascaret_exp.o test8_rezo.o test1_mascaret_imp.o test5_mascaret_exp.o test8_sarap.o test16_sarap2.o test3_mascaret_imp.o test11_sarap.o
clean: neat
	-rm -f .cppdefs $(OBJ) ../../bin/$(GP)
neat:
	-rm -f $(TMPFILES)
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
gp_masc: $(OBJ) 
	$(LD) $(OBJ) $(LDFLAGS) -o ../../bin/gp_masc  $(LDFLAGSLIB)
gp_masc.exe: $(OBJ) 
	$(LD) $(OBJ) -o ../../bin/gp_masc.exe  $(LDFLAGSLIB)
gp_mascOSX: $(OBJ) 
	$(LD) $(OBJ) -o ../../bin/gp_mascOSX  $(LDFLAGSLIB)
