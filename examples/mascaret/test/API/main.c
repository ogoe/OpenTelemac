/* 
 * File:   main.c
 * 
 * Author: Fabrice ZAOUI
 * 
 * Testing the Global Performance and robustness of MASCARET API
 * 
 * Copyright EDF 2014
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "cte_GP.h"

/* Prototypes */
#include "TestList.h"

#define NB_OUTER_LOOP 1
#define NB_INNER_LOOP 1
#define NB_TEST 54


int main(int argc, char** argv) {

    int   i,j;             /* loop indices */
    int   err;             /* error flag */
    time_t start,finish;   /* time recording */
    double elapsed_time;   /* duration */
    
    /* Test for the right number of arguments */
    if(argc!=2)
    {
        printf("\n --> Error using GP_Mascaret!\n");
        printf("\n Wrong number of arguments, usage :\n\t> gp_masc.exe PathDir\n");
        printf("\twhere 'PathDir' is the path of the parent directory for all the test files of MASCARET\n");
        exit(0);
    }
    time(&start);

    for(j=1;j<=NB_OUTER_LOOP;j++)
    {
        for(i=1;i<=NB_TEST;i++)
        {
           switch(i)
           {
              case(1) :
                 err = test1_mascaret_exp(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 1 failed\n");
                     exit(1);
                 }
                 break;
                 
              case(2) :
                 err = test1_mascaret_imp(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 2 failed\n");
                     exit(1);
                 }
                 break;
                 
              case(3) :
                 err = test1_sarap(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 3 failed\n");
                     exit(1);
                 }
                 break;
                 
              case(4) :
                 err = test2_mascaret_exp(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 4 failed\n");
                     exit(1);
                 }
                 break;   
              
              case(5) :
                 err = test2_sarap(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 5 failed\n");
                     exit(1);
                 }
                 break;      
              
              case(6) :
                 err = test3_mascaret_exp(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 6 failed\n");
                     exit(1);
                 }
                 break;    
               
              case(7) :
                 err = test3_mascaret_imp(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 7 failed\n");
                     exit(1);
                 }
                 break;      
              
              case(8) :
                 err = test3_sarap(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 8 failed\n");
                     exit(1);
                 }
                 break;       
                 
              case(9) :
                 err = test4_sarap(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 9 failed\n");
                     exit(1);
                 }
                 break;       
              
              case(10) :
                 err = test5_mascaret_exp(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 10 failed\n");
                     exit(1);
                 }
                 break;      
              
              case(11) :
                 err = test5_sarap(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 11 failed\n");
                     exit(1);
                 }
                 break;      
              
              case(12) :
                 err = test6_mascaret1(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 12 failed\n");
                     exit(1);
                 }
                 break;  
                 
              case(13) :
                 err = test6_mascaret2(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 13 failed\n");
                     exit(1);
                 }
                 break;  
                 
              case(14) :
                 err = test7_rezo(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 14 failed\n");
                     exit(1);
                 }
                 break;  
                 
              case(15) :
                 err = test7_sarap(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 15 failed\n");
                     exit(1);
                 }
                 break;  
                 
              case(16) :
                 err = test8_rezo(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 16 failed\n");
                     exit(1);
                 }
                 break;    
                 
              case(17) :
                 err = test8_sarap(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 17 failed\n");
                     exit(1);
                 }
                 break;  
                 
              case(18) :
                 err = test9_rezo(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 18 failed\n");
                     exit(1);
                 }
                 break;     
                 
              case(19) :
                 err = test9_sarap(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 19 failed\n");
                     exit(1);
                 }
                 break;        
                 
              case(20) :
                 err = test10_mascaret(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 20 failed\n");
                     exit(1);
                 }
                 break;  
                 
              case(21) :
                 err = test10_sarap(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 21 failed\n");
                     exit(1);
                 }
                 break;  
                 
              case(22) :
                 err = test11_rezo(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 22 failed\n");
                     exit(1);
                 }
                 break;  
              
              case(23) :
                 err = test11_sarap(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 23 failed\n");
                     exit(1);
                 }
                 break;    
                 
              case(24) :
                 err = test12_mascaret_exp(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 24 failed\n");
                     exit(1);
                 }
                 break;     
                 
              case(25) :
                 err = test12_mascaret_imp(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 25 failed\n");
                     exit(1);
                 }
                 break;   
                 
              case(26) :
                 err = test12_rezo(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 26 failed\n");
                     exit(1);
                 }
                 break;   
                 
              case(27) :
                 err = test13_mascaret(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 27 failed\n");
                     exit(1);
                 }
                 break;   
                 
              case(28) :
                 err = test13_rezo(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 28 failed\n");
                     exit(1);
                 }
                 break;      
                 
              case(29) :
                 err = test14_rezo(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 29 failed\n");
                     exit(1);
                 }
                 break;     
                 
              case(30) :
                 err = test15_mascaret(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 30 failed\n");
                     exit(1);
                 }
                 break;     
                 
              case(31) :
                 err = test15_rezo1(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 31 failed\n");
                     exit(1);
                 }
                 break;     
                 
              case(32) :
                 err = test15_rezo2(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 32 failed\n");
                     exit(1);
                 }
                 break;     
                 
              case(33) :
                 err = test16_mascaret1(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 33 failed\n");
                     exit(1);
                 }
                 break;    
                 
              case(34) :
                 err = test16_mascaret2(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 34 failed\n");
                     exit(1);
                 }
                 break;       
                 
              case(35) :
                 err = test16_sarap1(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 35 failed\n");
                     exit(1);
                 }
                 break;        
                 
              case(36) :
                 err = test16_sarap2(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 36 failed\n");
                     exit(1);
                 }
                 break;        
                 
              case(37) :
                 err = test17_mascaret(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 37 failed\n");
                     exit(1);
                 }
                 break;        
                 
              case(38) :
                 err = test17_sarap(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 38 failed\n");
                     exit(1);
                 }
                 break;           
                 
              case(39) :
                 err = test18_mascaret(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 39 failed\n");
                     exit(1);
                 }
                 break;           
                 
              case(40) :
                 err = test18_sarap(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 40 failed\n");
                     exit(1);
                 }
                 break;          
                 
              case(41) :
                 err = test19_mascaret(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 41 failed\n");
                     exit(1);
                 }
                 break;    
                 
              case(42) :
                 err = test19_sarap(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 42 failed\n");
                     exit(1);
                 }
                 break;    
                 
              case(43) :
                 err = test20_mascaret(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 43 failed\n");
                     exit(1);
                 }
                 break;    
                    
              case(44) :
                 err = test21_mascaret(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 44 failed\n");
                     exit(1);
                 }
                 break;       
                 
              case(45) :
                 err = test22_mascaret(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 45 failed\n");
                     exit(1);
                 }
                 break;       
                 
              case(46) :
                 err = test22_sarap(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 46 failed\n");
                     exit(1);
                 }
                 break;       
                 
              case(47) :
                 err = test23_mascaret(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 47 failed\n");
                     exit(1);
                 }
                 break;       
                 
              case(48) :
                 err = test24_mascaret_exp(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 48 failed\n");
                     exit(1);
                 }
                 break;       
                 
              case(49) :
                 err = test24_mascaret_imp(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 49 failed\n");
                     exit(1);
                 }
                 break;   

              case(50) :
                 err = test25_rezo1(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 50 failed\n");
                     exit(1);
                 }
                 break;                 
              
              case(51) :
                 err = test25_rezo2(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 51 failed\n");
                     exit(1);
                 }
                 break;       
              
              case(52) :
                 err = test26_casier(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 52 failed\n");
                     exit(1);
                 }
                 break;    
                 
              case(53) :
                 err = test27_casier(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 53 failed\n");
                     exit(1);
                 }
                 break;    
                 
              case(54) :
                 err = test28_mascaret(argv[1],NB_INNER_LOOP);
                 if(err)
                 {
                     printf("\n --> Test no. 54 failed\n");
                     exit(1);
                 }
                 break;    
                 
                 
              default: 
                 break; 

           }

        }
        time(&finish);
        elapsed_time = difftime(finish,start);
        printf("\r\t-- Loop no.%d done (%d simulations in %d seconds) --\n",j,j*NB_TEST*NB_INNER_LOOP,(int) elapsed_time);fflush(stdout);
    }

   /* process time evaluation */
   time(&finish);
   elapsed_time = difftime(finish,start);
   printf("--> All the %d tests successfully done in %d seconds\n" ,NB_TEST,(int) elapsed_time);
   printf("--> Total number of MASCARET simulations : %d\n",NB_TEST*NB_OUTER_LOOP*NB_INNER_LOOP);
   fflush(stdout);
    
    return (EXIT_SUCCESS);
}

