       IDENTIFICATION DIVISION.
       PROGRAM-ID. US-11-ARCHIVER-BULLETINS.
      * US-11 : Archiver les bulletins de paie
      * Feature 5 : Historique des paies
      * PI-2 : Systeme de Gestion des Employes

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-BULLETINS
               ASSIGN TO
               '../../../PI-1/feature-paie/bulletins.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FICHIER-HISTORIQUE
               ASSIGN TO '../historique-paies.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
        FD FICHIER-BULLETINS.
        01 LIGNE-BULLETIN     PIC X(80).

        FD FICHIER-HISTORIQUE.
        01 LIGNE-HISTORIQUE   PIC X(80).

       WORKING-STORAGE SECTION.
        01 WS-FIN             PIC X VALUE 'N'.
        01 WS-COMPTEUR        PIC 9(3) VALUE 0.
        01 WS-DATE-ARCH       PIC X(10) VALUE '2026-03-10'.

       PROCEDURE DIVISION.

           OPEN INPUT  FICHIER-BULLETINS
           OPEN OUTPUT FICHIER-HISTORIQUE

           MOVE SPACES TO LIGNE-HISTORIQUE
           STRING '== ARCHIVE DU ' WS-DATE-ARCH ' =='
               DELIMITED SIZE INTO LIGNE-HISTORIQUE
           WRITE LIGNE-HISTORIQUE

           PERFORM UNTIL WS-FIN = 'O'
               READ FICHIER-BULLETINS
                   AT END MOVE 'O' TO WS-FIN
                   NOT AT END
                       ADD 1 TO WS-COMPTEUR
                       MOVE LIGNE-BULLETIN TO LIGNE-HISTORIQUE
                       WRITE LIGNE-HISTORIQUE
               END-READ
           END-PERFORM.

           MOVE '======================================' TO
               LIGNE-HISTORIQUE
           WRITE LIGNE-HISTORIQUE

           CLOSE FICHIER-BULLETINS
           CLOSE FICHIER-HISTORIQUE
           DISPLAY 'Archive generee : historique-paies.txt'
           DISPLAY 'Lignes archivees : ' WS-COMPTEUR
           STOP RUN.