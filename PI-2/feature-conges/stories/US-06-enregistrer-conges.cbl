       IDENTIFICATION DIVISION.
       PROGRAM-ID. US-06-ENREGISTRER-CONGES.
      * US-06 : Enregistrer les conges
      * Feature 3 : Gestion des conges
      * PI-2 : Systeme de Gestion des Employes

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-CONGES
               ASSIGN TO '../conges.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
        FD FICHIER-CONGES.
           COPY '../../../copybooks/conge.cpy'.

       WORKING-STORAGE SECTION.
        01 WS-FIN           PIC X     VALUE 'N'.
        01 WS-COMPTEUR      PIC 9(3)  VALUE 0.

       PROCEDURE DIVISION.

           OPEN INPUT FICHIER-CONGES

           DISPLAY '================================'
           DISPLAY ' LISTE DES CONGES               '
           DISPLAY '================================'

           PERFORM UNTIL WS-FIN = 'O'
               READ FICHIER-CONGES
                   AT END MOVE 'O' TO WS-FIN
                   NOT AT END
                       ADD 1 TO WS-COMPTEUR
                       DISPLAY '--------------------------------'
                       DISPLAY 'Employe  : ' CONGE-EMP-ID
                       DISPLAY 'Type     : ' CONGE-TYPE
                       DISPLAY 'Debut    : ' CONGE-DEBUT
                       DISPLAY 'Fin      : ' CONGE-FIN
                       DISPLAY 'Jours    : ' CONGE-NB-JOURS
               END-READ
           END-PERFORM.

           DISPLAY '================================'
           DISPLAY 'Total conges enregistres : '
               WS-COMPTEUR
           CLOSE FICHIER-CONGES
           STOP RUN.