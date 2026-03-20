       IDENTIFICATION DIVISION.
       PROGRAM-ID. US-01-LIRE-EMPLOYES.
      * US-01 : Lire les employes depuis un fichier
      * Feature : Calcul de la paie
      * PI-1 : Systeme de Gestion des Employes

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-EMPLOYES
               ASSIGN TO '../employes.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
        FD FICHIER-EMPLOYES.
        COPY '../../../copybooks/employe.cpy'.

       WORKING-STORAGE SECTION.
        01 WS-FIN           PIC X VALUE 'N'.
        01 WS-COMPTEUR      PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.

           OPEN INPUT FICHIER-EMPLOYES

           DISPLAY '================================'
           DISPLAY ' LISTE DES EMPLOYES             '
           DISPLAY '================================'

           PERFORM UNTIL WS-FIN = 'O'
               READ FICHIER-EMPLOYES
                   AT END MOVE 'O' TO WS-FIN
                   NOT AT END
                       ADD 1 TO WS-COMPTEUR
                       DISPLAY 'ID       : ' EMP-ID
                       DISPLAY 'Nom      : ' EMP-NOM
                       DISPLAY 'Prenom   : ' EMP-PRENOM
                       DISPLAY 'Salaire  : ' EMP-SALAIRE
                       DISPLAY 'Categorie: ' EMP-CATEGORIE
                       DISPLAY '--------------------------------'
               END-READ
           END-PERFORM.

           DISPLAY 'Total employes : ' WS-COMPTEUR
           CLOSE FICHIER-EMPLOYES
           STOP RUN.