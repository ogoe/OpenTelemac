//== Copyright (C) 2000-2017 EDF-CEREMA ==
//
//   This file is part of MASCARET.
//
//   MASCARET is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   MASCARET is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>
//

//***********************************************************************
// PROGICIEL : MASCARET        J.-M. LACOMBE
//
// VERSION : 8.1.3              EDF-CEREMA
//***********************************************************************
/************************************************************************
 *
 * IMPLEMENTAION DE L'API
 *
 ************************************************************************/
 #include <stdio.h>
 #include <stdlib.h>
 #include <string.h>
 #include "apimascaret.h"


 // Signatures des subroutines fortrans utilisees
 // Elles peuvent dependre legerement du compilateur utilise
 // Dans le cas de gfortran : le nom est en minuscule et un underscore a la fin du nom.
   void create_mascaret_(int *Erreur, int *Identifiant);
   void delete_mascaret_(int *Erreur, int *Identifiant);
   void save_etat_mascaret_(int *Erreur, int *Identifiant, int *IdentifiantEtat);
   void set_etat_mascaret_(int *Erreur, int *Identifiant, int *IdentifiantEtat);
   void free_all_save_etat_mascaret_(int *Erreur, int *Identifiant);
   void free_save_etat_mascaret_(int *Erreur, int *IdentifiantEtat);
   void get_erreur_mascaret_(int *Erreur, int *Identifiant, char *Message);
   void import_modele_mascaret_(int *Erreur, int *Identifiant, char TabNomFichier[], char TypeNomFichier[], int *Taille, int *Impression);
   void init_etat_mascaret_(int *Erreur, int *Identifiant, char *NomFichier, int *Impression);
   void init_ligne_mascaret_(int *Erreur, int *Identifiant, double Q[], double Z[], int *Taille);
   void calcul_mascaret_(int *Erreur, int *Identifiant, double *TpsInitial, double *TpsFinal, double *PasTps, int *Impression);
   void get_nb_condition_limite_mascaret_(int *Erreur, int *Identifiant, int *NbCL);
   void get_nom_condition_limite_mascaret_(int *Erreur, int *Identifiant, int *NumCL, char *NomCL, int *NumLoi);
   void calcul_mascaret_condition_limite_(int *Erreur, int *Identifiant, double *TpsInitial, double *TpsFinal, double *PasTps, double TpsCl[], int *TailleTpsCL, double *CL1, double *CL2, int *Impression);
   void get_nb_var_mascaret_(int *NbVarMascaret);
   void get_desc_var_mascaret_(int *Erreur, int *Identifiant, char TabNom[], char TabDesc[], int *Taille);
   void get_type_var_mascaret_(int *Erreur, int *Identifiant, char *NomVar, char *TypeVar, char *Categorie, int *Modifiable, int *dimVar);
   void get_taille_var_mascaret_(int *Erreur, int *Identifiant, char *NomVar, int *index1, int *taille1, int *taille2, int *taille3);
   void set_taille_var_mascaret_(int *Erreur, int *Identifiant, char *NomVar, int *index1, int *taille1, int *taille2, int *taille3);
   void get_double_mascaret_(int *Erreur, int *Identifiant, char *NomVar, int *index1, int *index2, int *index3, double *valeur);
   void get_int_mascaret_(int *Erreur, int *Identifiant, char *NomVar, int *index1, int *index2, int *index3, int *valeur);
   void get_bool_mascaret_(int *Erreur, int *Identifiant, char *NomVar, int *index1, int *index2, int *index3, int *valeur);
   void get_string_mascaret_(int *Erreur, int *Identifiant, char *NomVar, int *index1, int *index2, int *index3, char *valeur);
   void set_double_mascaret_(int *Erreur, int *Identifiant, char *NomVar, int *index1, int *index2, int *index3, double *valeur);
   void set_int_mascaret_(int *Erreur, int *Identifiant, char *NomVar, int *index1, int *index2, int *index3, int *valeur);
   void set_bool_mascaret_(int *Erreur, int *Identifiant, char *NomVar, int *index1, int *index2, int *index3, int *valeur);
   void set_string_mascaret_(int *Erreur, int *Identifiant, char *NomVar, int *index1, int *index2, int *index3, char *valeur);
   void version_mascaret_(int *Majeur, int *Mineur, int *Micro);
   void import_xml_(int *Erreur, int *Identifiant, char *NomFichier, int *importModele);
   void export_xml_(int *Erreur, int *Identifiant, char *NomFichier, int *AvecDesc, int *exportModele);
   void export_xml_saint_venant_(int *Erreur, int *Identifiant, char *NomFichier);
   void ouverture_balise_xml_(int *Erreur, int *Identifiant, char *NomFichier, int *uniteLogique, char *balise);
   void export_var_xml_(int *Erreur, int *Identifiant, int *uniteLogique, char *nomVar, int *avecDesc);
   void export_uservar_xml_(int *Erreur, int *Identifiant, int *uniteLogique, char *nomUserVar, char *typeVar, char *descVar, char *valeurVar);
   void fermeture_balise_xml_(int *Erreur, int *Identifiant, int *uniteLogique, char *balise);

 //Signatures des fonction utilitaires de conversion de chaine Fortran---C
   void converChaineFversC(char fortran[], int tailleFortran, char **chaineC);
   void converChaineCversF(char *chaineC, int malloc, char *fortran, int tailleFortran);

   //.................................................................................................................................
   // Initialise les ressources associees a une instance de Mascaret (Modele et Etat)
   // Retourne l'identifiant de l'instance
   // Argument en sortie :
   //      Identifiant de l'instance Mascaret utilise en argument des autres fonctions de l'API
   // .................................................................................................................................
   int C_CREATE_MASCARET(int *Identifiant)
   {
      int erreurFortran;
      int idFortran;
      // Appel API fortran
      create_mascaret_(&erreurFortran, &idFortran);
      *Identifiant = idFortran;
      return erreurFortran;
   }


   //.................................................................................................................................
   // Libere les ressources associees a une instance de Mascaret (Modele et Etat)
   // RQ : Depend de l'instance du modele ou de l'etat
   // Argument en entree :
   //     Identifiant :  Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   // .................................................................................................................................
   int C_DELETE_MASCARET(int Identifiant)
   {
      int erreurFortran;
      delete_mascaret_(&erreurFortran, &Identifiant);

      return erreurFortran;
   }

   //.................................................................................................................................
   // Sauvegarde en memoire de l'etat courant d'une instance de Mascaret pour un usage ulterieur
   // Argument en entree :
   //      Identifiant :  Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   // Argument en sortie :
   //      IdentifiantEtat : identifiant de l'etat de l'instance Mascaret sauvegarde
   //.................................................................................................................................
   int C_SAVE_ETAT_MASCARET(int Identifiant, int *IdentifiantEtat)
   {
      int erreurFortran;
      int idFortranEtat;
      save_etat_mascaret_(&erreurFortran, &Identifiant, &idFortranEtat);
      *IdentifiantEtat = idFortranEtat;
      return erreurFortran;
   }

   //.................................................................................................................................
   // Initialise l'etat courant d'une instance de Mascaret a partir d'un etat prealablement sauvegarde par SAVE_ETAT_MASCARET
   // Supprime la sauvegarde de l'etat.
   // Arguments en entree :
   //      Identifiant     :  Identifiant de l'instance de Mascaret dont l'etat va etre modifie
   //      IdentifiantEtat :  Identifiant de l'etat Mascaret sauvegarde par SAVE_ETAT_MASCARET
   //.................................................................................................................................
   int C_SET_ETAT_MASCARET(int Identifiant, int IdentifiantEtat)
   {
      int erreurFortran;
      set_etat_mascaret_(&erreurFortran, &Identifiant, &IdentifiantEtat);

      return erreurFortran;

   }
   //.................................................................................................................................
   // Desallocation de tous les etats sauvegardes concernant un identifiant Mascaret
   // Arguments en entree :
   //      Identifiant     :  Identifiant de l'instance de Mascaret dont les etats sauvegardes vont etre supprimes
   //.................................................................................................................................
   int C_FREE_ALL_SAVE_ETAT_MASCARET(int Identifiant)
    {
      int erreurFortran;
      free_all_save_etat_mascaret_(&erreurFortran, &Identifiant);

      return erreurFortran;
   }
  
   //.................................................................................................................................
   // Desallocation d'un etat sauvegardes
   // Arguments en entree :
   //      IdentifiantEtat     :  Identifiant de l'etat Mascaret sauvegarde par SAVE_ETAT_MASCARET qui va etre supprime
   //.................................................................................................................................
   int C_FREE_SAVE_ETAT_MASCARET(int IdentifiantEtat)
   {
      int erreurFortran;
      free_save_etat_mascaret_(&erreurFortran, &IdentifiantEtat);

      return erreurFortran;
   }

   // .................................................................................................................................
   // Retourne le message d'une erreur
   // RQ : Depend de l'instance du modele ou de l'etat
   // Argument en entree :
   //    Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   // Argument en sortie :
   //    Message : Message d'erreur en francais de la derniere fonction appelee (max 256 caracteres)
   // .................................................................................................................................
   int C_GET_ERREUR_MASCARET(int Identifiant, char **Message)
   {
      char messageFortran[256];
      int erreurFortran;

      get_erreur_mascaret_(&erreurFortran, &Identifiant, messageFortran);

      converChaineFversC(messageFortran, 256, Message);

      return erreurFortran;
   }


   //.................................................................................................................................
   // Importation d'un modele mascaret a partir des fichiers natifs de Mascaret
   // Arguments en entree :
   //       Identifiant    : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //       TabNomFichier  : Tableau des noms des fichiers natifs Mascaret a importer (un nom de fichier ne doit pas depasser 255 caracteres)
   //       TypeNomFichier : Tableau des type des fichiers natifs Mascaret a importer, valeurs acceptees :
   //                        "abaques", "dico", "casier", "geo", "loi", "cas","listing","damocle", "res", "listing_casier", "listing_liaison",
   //                        "res_casier", "res_liaison"
   //       Taille         : Taille des 2 tableaux TabNomFichier et TypeNomFichier
   //       Impression     : impression sur les fichiers listing (1-> Vrai 0-> Faux)
   // .................................................................................................................................
  int C_IMPORT_MODELE_MASCARET(int Identifiant, char *TabNomFichier[], char *TypeNomFichier[], int Taille, int Impression)
  {
     int erreurFortran;
     int i;
     char *tabNomFichierFortran;
     char *typeNomFichierFortran;

     tabNomFichierFortran = (char*)malloc( (Taille*255) * sizeof( char ) );
     for(i=0; i<Taille; i++)
     {
        converChaineCversF(TabNomFichier[i], 0, &tabNomFichierFortran[i*255], 255);
     }

     typeNomFichierFortran = (char*)malloc( (Taille*40) * sizeof( char ) );
     for(i=0; i<Taille; i++)
     {
        converChaineCversF(TypeNomFichier[i], 0, &typeNomFichierFortran[i*40], 40);
     }

     import_modele_mascaret_(&erreurFortran, &Identifiant, tabNomFichierFortran, typeNomFichierFortran, &Taille, &Impression);

     return erreurFortran;
  }


   //.................................................................................................................................
   // Importation de l'etat Mascaret a partir du fichier natif contenant la ligne d'eau initiale
   // Arguments en entree :
   //       Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //       NomFichier  : Nom du fichier natif contenant la ligne d'eau initiale (max 255 carateres)
   //       Impression  : impression sur les fichiers listing (1-> Vrai 0-> Faux)
   // .................................................................................................................................
  int C_INIT_ETAT_MASCARET(int Identifiant, char *NomFichier, int Impression)
  {
     int erreurFortran;
     char nomFichierFortran[255];

     converChaineCversF(NomFichier, 0, nomFichierFortran, 255);

     init_etat_mascaret_(&erreurFortran, &Identifiant, nomFichierFortran, &Impression);
     return erreurFortran;
  }

   //.................................................................................................................................
   // Importation de l'etat Mascaret a partir de la ligne d'eau initiale (debit, cote) passee en argument
   // Arguments en entree :
   //       Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //       Q           : Tableau des debits de la ligne d'eau initiale
   //       Z           : Tableau des cotes de la ligne d'eau initiale
   //       Taille      : Taille des 2 tableaux Q et Z
   // .................................................................................................................................
  int C_INIT_LIGNE_MASCARET(int Identifiant, double Q[], double Z[], int Taille)
  {
     int erreurFortran;

     init_ligne_mascaret_(&erreurFortran, &Identifiant, Q, Z, &Taille);


     return erreurFortran;
  }

   //.................................................................................................................................
   // Calcul d'un nouvel etat au "TpsFinal" en utilisant le modele courant et l'etat precedent
   // Arguments en entree :
   //       Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //       TpsInitial  : Temps initiale du calcul
   //       TpsFinal    : Temps final du calcul
   //       PasTps      : Pas de temps interne du calcul
   //       Impression  : impression sur les fichiers listing (1-> Vrai 0-> Faux)
   // .................................................................................................................................
  int C_CALCUL_MASCARET(int Identifiant, double TpsInitial, double TpsFinal, double PasTps, int Impression)
  {
     int erreurFortran;

     calcul_mascaret_(&erreurFortran, &Identifiant, &TpsInitial, &TpsFinal, &PasTps, &Impression);

     return erreurFortran;
  }

   //.................................................................................................................................
   // Recupere le nombre de condition limite dans le modele
   // Argument en entree :
   //        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   // Arguments en sortie :
   //        NbCL : le nombre de condition limite dans le modele
   //.................................................................................................................................
  int C_GET_NB_CONDITION_LIMITE_MASCARET(int Identifiant, int *NbCL)
  {
     int erreurFortran;

     get_nb_condition_limite_mascaret_(&erreurFortran, &Identifiant, NbCL);

     return erreurFortran;
  }

   //.................................................................................................................................
   // Recupere le nom d'une condition limite du modele
   // Argument en entree :
   //        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NumCL       : Le numero de la condition limite dont on veut connaitre le nom
   // Arguments en sortie :
   //        NbCL   : le nombre de condition limite dans le modele
   //        NumLoi : Numero de la Loi correspondant a la condition limite dans le modele
   //.................................................................................................................................
  int C_GET_NOM_CONDITION_LIMITE_MASCARET(int Identifiant, int NumCL, char **NomCL, int *NumLoi)
  {
     int erreurFortran;
     char messageFortran[30];

     get_nom_condition_limite_mascaret_(&erreurFortran, &Identifiant, &NumCL, messageFortran, NumLoi);

     converChaineFversC(messageFortran, 30, NomCL);

     return erreurFortran;
  }

   //.................................................................................................................................
   // Calcul d'un nouvel etat au "TpsFinal" en utilisant le modele courant, les nouvelles contions limites et l'etat precedent
   // Arguments en entree :
   //       Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //       TpsInitial  : Temps initiale du calcul
   //       TpsFinal    : Temps final du calcul
   //       PasTps      : Pas de temps interne du calcul
   //       TpsCl       : Vecteur temps des nouvelles conditions limites
   //       TailleTpsCL : Nombre d'element du vecteur temps des nouvelles conditions limites
   //       Cl1         : Matrice de la premiere composante des nouvelles conditions limites
   //       Cl2         : Matrice de la deuxieme composante des nouvelles conditions limites
   //       Impression  : impression sur les fichiers listing (1-> Vrai 0-> Faux)
   // .................................................................................................................................
  int C_CALCUL_MASCARET_CONDITION_LIMITE(int Identifiant, double TpsInitial, double TpsFinal, double PasTps, double TpsCl[], int TailleTpsCL, double **Cl1, double **Cl2, int Impression)
  {
     int erreurFortran;
     double *Cl1Fortran, *Cl2Fortran;
     int NbCL, i, j, indexFortran;

     get_nb_condition_limite_mascaret_(&erreurFortran, &Identifiant, &NbCL);
     
     Cl1Fortran = (double*)malloc( (NbCL*TailleTpsCL) * sizeof( double ) );
     Cl2Fortran = (double*)malloc( (NbCL*TailleTpsCL) * sizeof( double ) );
     indexFortran = 0;
     for(i=0; i<NbCL; i++) {
         for (j=0; j<TailleTpsCL; j++) {
             Cl1Fortran[indexFortran] = Cl1[i][j];
             Cl2Fortran[indexFortran] = Cl2[i][j];
             indexFortran++;
         }
     }
         
     
     calcul_mascaret_condition_limite_(&erreurFortran, &Identifiant, &TpsInitial, &TpsFinal, &PasTps,
                                       TpsCl, &TailleTpsCL, Cl1Fortran, Cl2Fortran, &Impression);
     
     free(Cl1Fortran);
     free(Cl2Fortran);

     return erreurFortran;
  }


  //.................................................................................................................................
   // Recupere la liste des variables de Mascaret accompagnee d'une description
   // RQ : Ne depend pas de l'instance du modele ou de l'etat
   // Argument en entree :
   //        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   // Arguments en sortie :
   //        TabNom      : Tableau des noms de variable du modele ou de l'etat
   //        TabDesc     : Tableau des descriptions de variable du modele ou de l'etat
   //        Taille      : Taille des tableaux des noms et des descriptions de variable
   //.................................................................................................................................
  int C_GET_DESC_VAR_MASCARET(int Identifiant, char **TabNom[], char **TabDesc[], int *Taille)
  {
     int NbVarMascaret;
     int erreurFortran;
     int i,j;
     char *tabNomFortran;
     char *tabDescFortran;
     char tmpVarFortran[40];
     char tmpDescFortran[110];

     get_nb_var_mascaret_(&NbVarMascaret);
     
     tabNomFortran = (char*)malloc( (NbVarMascaret*40) * sizeof( char ) );
     tabDescFortran = (char*)malloc( (NbVarMascaret*110) * sizeof( char ) );

     *TabNom  = (char**)malloc(NbVarMascaret* sizeof( char *) );
     *TabDesc = (char**)malloc(NbVarMascaret* sizeof( char *) );


     get_desc_var_mascaret_(&erreurFortran, &Identifiant, tabNomFortran, tabDescFortran, &NbVarMascaret);

     for (i=0 ; i < NbVarMascaret ; i++)
     {    
         for (j=0 ; j < 40 ; j++)
         {
             tmpVarFortran[j] = tabNomFortran[(40*i)+j];         
         }   
         converChaineFversC(tmpVarFortran, 40, &(*TabNom)[i]);

         for (j=0 ; j < 110 ; j++)
         {
             tmpDescFortran[j] = tabDescFortran[(110*i)+j];
         }
         converChaineFversC(tmpDescFortran, 110, &(*TabDesc)[i]);
     }

     free(tabNomFortran);
     free(tabDescFortran);
     *Taille = NbVarMascaret;
     return erreurFortran;
  }


   // .................................................................................................................................
   // Recupere des informations sur une variable (identifie par NomVar) :
   //   * le type (reel, entier, booleen, chaine de caractere),
   //   * la categorie (modele ou etat)
   //   * si la variable est modifiable dans l'API avec SET_XXXX_MASCARET
   //   * le nombre d'indexe pour acceder a la variable
   // RQ : Ne depend pas de l'instance du modele ou de l'etat
   // Arguments en entree :
   //        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   // Arguments en sortie :
   //        TypeVar     : valeurs possibles :"INT" ou "DOUBLE" ou "BOOL" ou "STRING" ou "TABINT" ou "TABDOUBLE" ou "TABBOOL"
   //        Categorie   : valeurs possibles : "MODEL" ou "STATE"
   //        Modifiable  : Si vrai alors on peut utiliser une fonction SET_XXXX_MASCARET sur la variable
   //        dimVar      : dimension (c'est a dire le nombre d'indexe de 0 a 3)
   // .................................................................................................................................
   int C_GET_TYPE_VAR_MASCARET(int Identifiant, char *NomVar, char **TypeVar, char **Categorie, int *Modifiable, int *dimVar)
  {
     int erreurFortran;
     char nomVarFortran[40];
     char typeVarFortran[10], categorieFortran[10];

     converChaineCversF(NomVar, 0, nomVarFortran, 40);

     get_type_var_mascaret_(&erreurFortran, &Identifiant, nomVarFortran, typeVarFortran, categorieFortran, Modifiable, dimVar);

     converChaineFversC(typeVarFortran, 10, TypeVar);
     converChaineFversC(categorieFortran, 10, Categorie);

     return erreurFortran;
  }

   // .................................................................................................................................
   // Accede a la taille maximum des indexes pour acceder a une variable
   // RQ : Depend de l'instance du modele ou de l'etat
   // Arguments en entree :
   //        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //        index1      : valeur du 1er indice utilise pour Profils, Lois, Singularites, Deversoirs, Extremites, Casiers et Confluents
   // Arguments en sortie :
   //        taille1     : valeur max du 1er indice
   //        taille2     : valeur max du 2e  indice
   //        taille3     : valeur max du 3e  indice
   // .................................................................................................................................
  int C_GET_TAILLE_VAR_MASCARET(int Identifiant, char *NomVar, int index1, int *taille1, int *taille2, int *taille3)
  {
     int erreurFortran;
     char nomVarFortran[40];

     converChaineCversF(NomVar, 0, nomVarFortran, 40);

     get_taille_var_mascaret_(&erreurFortran, &Identifiant, nomVarFortran, &index1, taille1, taille2, taille3);

     return erreurFortran;
  }
  
   // .................................................................................................................................
   // Definit la taille maximum des indexes pour acceder a une variable
   // RQ : Depend de l'instance du modele ou de l'etat
   // Arguments en entree :
   //        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //        index1      : valeur du 1er indice utilise pour Profils, Lois, Singularites, Deversoirs, Extremites, Casiers et Confluents
   //        taille1     : valeur max du 1er indice
   //        taille2     : valeur max du 2e  indice
   //        taille3     : valeur max du 3e  indice
   // .................................................................................................................................
   int C_SET_TAILLE_VAR_MASCARET(int Identifiant, char *NomVar, int index1,
          int taille1, int taille2, int taille3) 
   {
       int erreurFortran;
       char nomVarFortran[40];

       converChaineCversF(NomVar, 0, nomVarFortran, 40);

       set_taille_var_mascaret_(&erreurFortran, &Identifiant, nomVarFortran,
             &index1, &taille1, &taille2, &taille3);

       return erreurFortran;
   }

   /*********************************************************************************************************************************
    *
    *  Accesseurs permettant d'acceder aux valeurs des variables d'une instance du modele ou de l'etat
    * RQ : Depend de l'instance du modele ou de l'etat
    *
    *********************************************************************************************************************************/
   // Lecture d'une variable de type reel
   // Arguments en entree :
   //        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //        index1      : valeur du 1er indice
   //        index2      : valeur du 2e indice
   //        index3      : valeur du 3e indice
   //        valeur      : valeur de la variable pour les indexes specifies
  int C_GET_DOUBLE_MASCARET(int Identifiant, char *NomVar, int index1, int index2, int index3, double *valeur)
  {
     int erreurFortran;
     char nomVarFortran[40];

     converChaineCversF(NomVar, 0, nomVarFortran, 40);

     get_double_mascaret_(&erreurFortran, &Identifiant, nomVarFortran, &index1, &index2, &index3, valeur);

     return erreurFortran;
  }

   // Lecture d'une variable de type entier
   // Arguments en entree :
   //        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //        index1      : valeur du 1er indice
   //        index2      : valeur du 2e indice
   //        index3      : valeur du 3e indice
   //        valeur      : valeur de la variable pour les indexes specifies
  int C_GET_INT_MASCARET(int Identifiant, char *NomVar, int index1, int index2, int index3, int *valeur)
  {
     int erreurFortran;
     char nomVarFortran[40];

     converChaineCversF(NomVar, 0, nomVarFortran, 40);

     get_int_mascaret_(&erreurFortran, &Identifiant, nomVarFortran, &index1, &index2, &index3, valeur);


     return erreurFortran;
  }

   // Lecture d'une variable de type booleen
   // Arguments en entree :
   //        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //        index1      : valeur du 1er indice
   //        index2      : valeur du 2e indice
   //        index3      : valeur du 3e indice
   //        valeur      : valeur de la variable pour les indexes specifies
  int C_GET_BOOL_MASCARET(int Identifiant, char *NomVar, int index1, int index2, int index3, int *valeur)
  {
     int erreurFortran;
     char nomVarFortran[40];

     converChaineCversF(NomVar, 0, nomVarFortran, 40);

     get_bool_mascaret_(&erreurFortran, &Identifiant, nomVarFortran, &index1, &index2, &index3, valeur);


     return erreurFortran;
  }

   // Lecture d'une variable de type chaine de caractere
   // Arguments en entree :
   //        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //        index1      : valeur du 1er indice
   //        index2      : valeur du 2e indice
   //        index3      : valeur du 3e indice
   //        valeur      : valeur de la variable pour les indexes specifies
  int C_GET_STRING_MASCARET(int Identifiant, char *NomVar, int index1, int index2, int index3, char **valeur)
  {
     int erreurFortran;
     char nomVarFortran[40];
     char valeurFortran[256];


     converChaineCversF(NomVar, 0, nomVarFortran, 40);

     get_string_mascaret_(&erreurFortran, &Identifiant, nomVarFortran, &index1, &index2, &index3, valeurFortran);

     converChaineFversC(valeurFortran, 256, valeur);

     return erreurFortran;
  }


   /*********************************************************************************************************************************
    *
    * Mutateurs permettant de modifier une valeur d'une variable d'une instance du modele ou de l'etat
    * RQ : Depend de l'instance du modele ou de l'etat
    *
    *********************************************************************************************************************************/
   // Modification d'une variable de type reel
   // Arguments en entree :
   //        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //        index1      : valeur du 1er indice
   //        index2      : valeur du 2e indice
   //        index3      : valeur du 3e indice
   //        valeur      : nouvelle valeur de la variable pour les indexes specifies
  int C_SET_DOUBLE_MASCARET(int Identifiant, char *NomVar, int index1, int index2, int index3, double valeur)
  {
     int erreurFortran;
     char nomVarFortran[40];

     converChaineCversF(NomVar, 0, nomVarFortran, 40);

     set_double_mascaret_(&erreurFortran, &Identifiant, nomVarFortran, &index1, &index2, &index3, &valeur);

     return erreurFortran;
  }

   // Modification d'une variable de type entier
   // Arguments en entree :
   //        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //        index1      : valeur du 1er indice
   //        index2      : valeur du 2e indice
   //        index3      : valeur du 3e indice
   //        valeur      : nouvelle valeur de la variable pour les indexes specifies
  int C_SET_INT_MASCARET(int Identifiant, char *NomVar, int index1, int index2, int index3, int valeur)
  {
     int erreurFortran;
     char nomVarFortran[40];

     converChaineCversF(NomVar, 0, nomVarFortran, 40);

     set_int_mascaret_(&erreurFortran, &Identifiant, nomVarFortran, &index1, &index2, &index3, &valeur);

     return erreurFortran;
  }

   // Modification d'une variable de type booleen
   // Arguments en entree :
   //        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //        index1      : valeur du 1er indice
   //        index2      : valeur du 2e indice
   //        index3      : valeur du 3e indice
   //        valeur      : nouvelle valeur de la variable pour les indexes specifies
  int C_SET_BOOL_MASCARET(int Identifiant, char *NomVar, int index1, int index2, int index3, int valeur)
  {
     int erreurFortran;
     char nomVarFortran[40];

     converChaineCversF(NomVar, 0, nomVarFortran, 40);

     set_bool_mascaret_(&erreurFortran, &Identifiant, nomVarFortran, &index1, &index2, &index3, &valeur);

     return erreurFortran;
  }

   // Modification d'une variable de type chaine de caractere
   // Arguments en entree :
   //        Identifiant : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NomVar      : Nom de la variable (notation pointe), la liste des variables est retournee par C_GET_DESC_VAR_MASCARET
   //        index1      : valeur du 1er indice
   //        index2      : valeur du 2e indice
   //        index3      : valeur du 3e indice
   //        valeur      : nouvelle valeur de la variable pour les indexes specifies
  int C_SET_STRING_MASCARET(int Identifiant, char *NomVar, int index1, int index2, int index3, char *valeur)
  {
     int erreurFortran;
     char nomVarFortran[40];
     char valeurFortran[256];

     converChaineCversF(NomVar, 0, nomVarFortran, 40);
     converChaineCversF(valeur, 0, valeurFortran, 256);

     set_string_mascaret_(&erreurFortran, &Identifiant, nomVarFortran, &index1, &index2, &index3, valeurFortran);

     return erreurFortran;
  }

/*************************************************************************************************************************************
 *
 *  Une fonction utilitaire
 *
 *************************************************************************************************************************************/
   //.................................................................................................................................
   // Retourne la version courante de Mascaret
   // Arguments en sortie :
   //        Majeur  : Numero de la version Majeur de Mascaret
		 //        Mineur  : Numero de la version Mineur de Mascaret
		 //        Micro   : Numero de la version Micro de Mascaret
   // .................................................................................................................................
  int C_VERSION_MASCARET(int *Majeur, int *Mineur, int *Micro)
  {
    version_mascaret_(Majeur, Mineur, Micro);
    return 0;
  }


/*************************************************************************************************************************************
 *
 *  Quelques fonctions utiles pour importer/exporter en XML des variables de Mascaret
 *
 *************************************************************************************************************************************/
   //.................................................................................................................................
   //  Import d'un modele Mascaret depuis un fichier XML
   //  Arguments en entree :
   //        Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NomFichier   : Nom du fichier XML cree contenant le Modele ou l'etat Mascaret (max 255 caracteres)
   //        importModele : si vrai (valeur 1), import du modele, sinon import de l'etat
   //.................................................................................................................................
   int C_IMPORT_XML(int Identifiant, char *NomFichier, int importModele)
   {
      int erreurFortran;
      char nomFichierFortran[255];

      converChaineCversF(NomFichier, 0, nomFichierFortran, 255);

      import_xml_(&erreurFortran, &Identifiant, nomFichierFortran, &importModele);

      return erreurFortran;
   }

  //.................................................................................................................................
   //  Export d'un modele Mascaret dans un fichier XML
   //  Arguments en entree :
   //        Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NomFichier   : Nom du fichier XML cree contenant le Modele ou l'etat Mascaret (max 255 caracteres)
   //        AvecDesc     : si vrai (valeur 1), ajoute la description de la variable
   //        exportModele : si vrai (valeur 1), exportation du modele, sinon export de l'etat
   //.................................................................................................................................
   int C_EXPORT_XML(int Identifiant, char *NomFichier, int AvecDesc, int exportModele)
   {
      int erreurFortran;
      char nomFichierFortran[255];

      converChaineCversF(NomFichier, 0, nomFichierFortran, 255);

      export_xml_(&erreurFortran, &Identifiant, nomFichierFortran, &AvecDesc, &exportModele);

      return erreurFortran;
   }

  //.................................................................................................................................
   //  Export d'un modele Mascaret dans un fichier XML
   //  Arguments en entree :
   //        Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NomFichier   : Nom du fichier XML cree contenant le Modele ou l'etat Mascaret (max 255 caracteres)
   //        AvecDesc     : si vrai (valeur 1), ajoute la description de la variable
   //        exportModele : si vrai (valeur 1), exportation du modele, sinon export de l'etat
   //.................................................................................................................................
   int C_EXPORT_XML_SAINT_VENANT(int Identifiant, char *NomFichier)
   {
      int erreurFortran;
      char nomFichierFortran[255];

      converChaineCversF(NomFichier, 0, nomFichierFortran, 255);

      export_xml_saint_venant_(&erreurFortran, &Identifiant, nomFichierFortran);

      return erreurFortran;
   }   
   
   
   //-------------------------------------------------------------------------------
   // Ouverture du fichier XML suivi de l'ouverture de la balise XML racine
   // Ce fichier est prevu pour contenir des variables au choix de Mascaret via l'utilisation de C_EXPORT_VAR_XML
   // Arguments en entree :
   //        Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        NomFichier   : Nom du fichier XML cree contenant l'entete du fichier XML (max 255 caracteres)
   //        uniteLogique : unite logique utilise par le fichier XML : doit etre strictement superieur a 36
   //        balise       : balise racine du fichier XML
   //-------------------------------------------------------------------------------
  int C_OUVERTURE_BALISE_XML(int Identifiant, char *NomFichier, int uniteLogique, char *balise)
  {
      int erreurFortran;
      char nomFichierFortran[255];
      char baliseFortran[255];

      converChaineCversF(NomFichier, 0, nomFichierFortran, 255);
      converChaineCversF(balise, 0, baliseFortran, 255);
      ouverture_balise_xml_(&erreurFortran, &Identifiant, nomFichierFortran, &uniteLogique, baliseFortran);

      return erreurFortran;
  }

   //-----------------------------------------------------------------------------------
   // Exportation d'une variable Mascaret dans un fichier XML
   // Avant d'utiliser cette procedure, il faut avoir deja execute OUVERTURE_BALISE_XML
   // et avoir initialiser le modele avec IMPORT_MODELE_MASCARET.
   // On peut ensuite utiliser plusieurs fois cette procedure
   // Arguments en entree :
   //        Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        uniteLogique : Unite logique utilise par le fichier XML - Doit egale a celle de l'ouverture (utilisation de C_OUVERTURE_BALISE_XML)
   //        nomVar       : Nom de la variable Mascaret a exporter
   //        avecDesc     : Si vrai (valeur 1), on ajoute une description en francais de la variable
   //-----------------------------------------------------------------------------------
  int C_EXPORT_VAR_XML(int Identifiant, int uniteLogique, char *nomVar, int avecDesc)
  {
     int erreurFortran;
     char nomVarFortran[40];

     converChaineCversF(nomVar, 0, nomVarFortran, 40);
     export_var_xml_(&erreurFortran, &Identifiant, &uniteLogique, nomVarFortran, &avecDesc);

     return erreurFortran;
  }

  
 //-----------------------------------------------------------------------------------
   // Exportation d'une variable utilisateur dans un fichier XML
   // par ex: <nomUserVar type="typeVar" description="descVar">valeurVar</nomUserVar>
   // Avant d'utiliser cette procedure, il faut avoir deja execute OUVERTURE_BALISE_XML
   // et avoir initialiser le modele avec IMPORT_MODELE_MASCARET.
   // On peut ensuite utiliser plusieurs fois cette procedure
   // Arguments en entree :
   //      Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //      uniteLogique : Unite logique utilise par le fichier XML - Doit egale a celle de l'ouverture (utilisation de C_OUVERTURE_BALISE_XML)
   //      nomUserVar   : Nom de la variable Utilisateur a exporter
   //      typeVar      : chaine du type de la variable "INT", ou "DOUBLE"
   //      descVar      : chaine de description de la variable
   //      valeurVar    : chaine de la valeur a ecrire dans la balise XML
   //-----------------------------------------------------------------------------------
  int C_EXPORT_USERVAR_XML(int Identifiant, int uniteLogique, char *nomUserVar, char *typeVar, char *descVar, char *valeurVar)
  {
     int erreurFortran;
     char nomVarFortran[40];
     char typeVarFortran[20];
     char descVarFortran[110];
     char valeurVarFortran[20];

     converChaineCversF(nomUserVar, 0, nomVarFortran, 40);
     converChaineCversF(typeVar, 0, typeVarFortran, 20);
     converChaineCversF(descVar, 0, descVarFortran, 110);
     converChaineCversF(valeurVar, 0, valeurVarFortran, 20);
     export_uservar_xml_(&erreurFortran, &Identifiant, &uniteLogique, nomVarFortran, typeVarFortran, descVarFortran, valeurVarFortran);

     return erreurFortran;
  }

  
   //------------------------------------------------------------------------------------
   // Fermeture du la balise XML racine puis fermeture du fichier
   // Doit etre utiliser apres OUVERTURE_BALISE_XML et normalement apres C_EXPORT_VAR_XML
   // Arguments en entree :
   //        Identifiant  : Identifiant de l'instance Mascaret retourne par "CREATE_MASCARET"
   //        uniteLogique : Unite logique utilise par le fichier XML - Doit egale a celle de l'ouverture (utilisation de C_OUVERTURE_BALISE_XML)
   //        balise       : balise racine du fichier XML - Doit egale a celle de l'ouverture (utilisation de C_OUVERTURE_BALISE_XML)
   //-----------------------------------------------------------------------------------
  int C_FERMETURE_BALISE_XML(int Identifiant, int uniteLogique, char *balise)
  {
      int erreurFortran;
      char baliseFortran[255];

      converChaineCversF(balise, 0, baliseFortran, 255);
      fermeture_balise_xml_(&erreurFortran, &Identifiant, &uniteLogique, baliseFortran);
      return erreurFortran;
  }

/*************************************************************************************************************************************
 *
 *  2 fonctions internes pour convertir les chaines C en chaines Fortran et inversement
 *
 *************************************************************************************************************************************/

   //----------------------------------------------------------------------------------------------------------------------------------
   // Conversion d'une chaine Fortran en chaine C
   // Arguments en entree :
   //        fortran       : la chaine fortran represente en interne par un tableau de caracteres avec des espaces a droites
   //        tailleFortran : taille du tableau 'fortran'
   // Argument en sortie :
   //        chaineC       : la chaine de caracteres convertie avec un '\0' a la fin de la chaine
   //-----------------------------------------------------------------------------------------------------------------------------------
  void converChaineFversC(char fortran[], int tailleFortran, char **chaine)
  {
     int iFortran;
     int iChaineC;
     int tailleChaine;
     char *chaineC;

     // Reperer le 1er caractere qui ne soit pas un espace a partir de la fin du tableau
     tailleChaine = 0;
     for(iFortran=(tailleFortran -1) ; iFortran >= 0 ; iFortran--)
     {
       if (fortran[iFortran]!= ' ')
       {
          tailleChaine = iFortran +1;
          break;
       }
     }
     chaineC = (char*)malloc( (tailleChaine+1) * sizeof( char ) );

     chaineC[tailleChaine]='\0';
     for(iChaineC=0 ; iChaineC< tailleChaine ; iChaineC++)
     {
       chaineC[iChaineC] = fortran[iChaineC];
     }

     *chaine = chaineC;
     
     return;
  }

   //----------------------------------------------------------------------------------------------------------------------------------
   // Conversion d'une chaine C en chaine Fortran
   // Integre l'allocation du tableau de char Fortran si malloc vaut 1
   // Arguments en entree :
   //        chaineC       : la chaine de caracteres convertie avec un '\0' a la fin de la chaine
   //        malloc        : si vaut 1, alloue le tableau fortran
   // Argument en sortie :
   //        fortran       : la chaine fortran represente en interne par un tableau de caracteres avec des espaces a droites
   //        tailleFortran : taille du tableau 'fortran'
   //-----------------------------------------------------------------------------------------------------------------------------------
  void converChaineCversF(char *chaineC, int malloc, char *fortran, int tailleFortran)
  {
     int iFortran;
     int iChaineC;
     int tailleChaineC;
     int tailleCopie;

     // determination de la longueur de la chaine C
     tailleChaineC = strlen(chaineC);

     // tailleCopie = Min(tailleChaine,tailleFortran)
     tailleCopie = tailleChaineC;
     if (tailleFortran < tailleChaineC)
     {
       tailleCopie = tailleFortran;
     }

     // Copie de la chaine C dans la chaine Fortran caractere par caractere
     for(iChaineC=0 ; iChaineC < tailleCopie ; iChaineC++)
     {
       fortran[iChaineC] = chaineC[iChaineC];
     }

     // Complete la chaine Fortran par des espaces (si necessaire)
     for(iFortran=tailleCopie ; iFortran< tailleFortran ; iFortran++)
     {
       fortran[iFortran] = ' ';
     }

     return;
  }
