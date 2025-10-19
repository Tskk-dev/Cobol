       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILEHANDLER.
       AUTHOR. JULIUS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CALC-FILE ASSIGN TO "calculations.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CALC-FILE.
       01  CALC-RECORD      PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS   PIC XX.
       01  WS-EOF           PIC X  VALUE 'N'.
       01  A-NUM            PIC 99.
       01  B-NUM            PIC 99.
       01  SUM-RES          PIC 999.
       01  CNT              PIC 99 VALUE 0.

       PROCEDURE DIVISION.
      *> cobol-lint CL002 main-para
       MAIN-PARA.
           OPEN INPUT CALC-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Cannot open calculations.txt"
               DISPLAY "Status: " WS-FILE-STATUS
               STOP RUN
           END-IF

           DISPLAY "CALCULATIONS FROM FILE"
           DISPLAY "----------------------------------------"

           PERFORM UNTIL WS-EOF = 'Y'
               READ CALC-FILE
                  AT END
                     MOVE 'Y' TO WS-EOF
                  NOT AT END
                     UNSTRING CALC-RECORD DELIMITED BY SPACE
                        INTO A-NUM, B-NUM
                     END-UNSTRING
                     ADD A-NUM TO B-NUM GIVING SUM-RES
                     DISPLAY A-NUM " + " B-NUM " = " SUM-RES
                     ADD 1 TO CNT
               END-READ
           END-PERFORM

           DISPLAY "----------------------------------------"
           DISPLAY "Total calculations: " CNT

           CLOSE CALC-FILE
           STOP RUN.
