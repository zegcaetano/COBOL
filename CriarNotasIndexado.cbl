      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CriarNotasIndexado.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "slnotas.cbl".
       DATA DIVISION.
       FILE SECTION.
           COPY "fdnotas.cbl".
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       PROGRAM-BEGIN.
       OPEN OUTPUT NOTAS-FILE.
       DISPLAY "O ficheiro 'notas.dat' foi criado"
       CLOSE NOTAS-FILE.
       PROGRAM-DONE.
       STOP RUN.
