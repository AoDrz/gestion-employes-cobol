       IDENTIFICATION DIVISION.
       PROGRAM-ID. US-12-CONSULTER-HISTORIQUE.
      * US-12 : Consulter l'historique des paies
      * Feature 5 : Historique des paies
      * PI-2 : Systeme de Gestion des Employes

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-HISTORIQUE
               ASSIGN TO '../historique-paies.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
        FD FICHIER-HISTORIQUE.
        01 LIGNE-HISTORIQUE   PIC X(80).

       WORKING-STORAGE SECTION.
        01 WS-FIN             PIC X VALUE 'N'.
        01 WS-COMPTEUR        PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.

           OPEN INPUT FICHIER-HISTORIQUE

           DISPLAY '================================'
           DISPLAY ' HISTORIQUE DES PAIES           '
           DISPLAY '================================'

           PERFORM UNTIL WS-FIN = 'O'
               READ FICHIER-HISTORIQUE
                   AT END MOVE 'O' TO WS-FIN
                   NOT AT END
                       ADD 1 TO WS-COMPTEUR
                       DISPLAY LIGNE-HISTORIQUE
               END-READ
           END-PERFORM.

           DISPLAY '================================'
           DISPLAY 'Total lignes lues : ' WS-COMPTEUR
           CLOSE FICHIER-HISTORIQUE
           STOP RUN.