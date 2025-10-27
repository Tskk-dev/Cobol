       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-REPORT.
       AUTHOR. Julius Castillejo.
       DATE-WRITTEN. 2025-10-21.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "STUDENT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO "REPORT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INFILE.
       01  STUDENT-RECORD.
           05  DEPT-CD             PIC X(3).
           05  SNO-IN              PIC X(3).
           05  SNAME-IN            PIC X(25).
           05  SUBJ                PIC X(6).
           05  GRD                 PIC 9V99.

       FD  OUTFILE.
       01  REPORT-LINE             PIC X(80).

       WORKING-STORAGE SECTION.

       01  PREV-DEPT-CD            PIC X(3) VALUE SPACES.
       01  PREV-SNO                PIC X(3) VALUE SPACES.
       01  CURR-SNO                PIC X(3).
       01  CURR-DEPT-CD            PIC X(3).
       01  GRADE-TOTAL             PIC 999V99 VALUE 0.
       01  SUBJECT-COUNT           PIC 99 VALUE 0.
       01  AVE-CALC                PIC 9V99.

       01  PCTR                    PIC 999 VALUE 0.
       01  FCTR                    PIC 999 VALUE 0.
       01  SCTR                    PIC 999 VALUE 0.

      * Overall counter
       01  OVSCTR                  PIC 9999 VALUE 0.

      * Flags
       01  EOF-FLAG                PIC X VALUE 'N'.
       01  FIRST-RECORD            PIC X VALUE 'Y'.


       01  DEPT-NAME               PIC X(30).
       01  STUDENT-NAME-HOLD       PIC X(25).


       01  HEADER-1.
           05  FILLER              PIC X(15) VALUE SPACES.
           05  FILLER              PIC X(50)
               VALUE "POLYTECHNIC UNIVERSITY OF THE PHILIPPINES".

       01  HEADER-2.
           05  FILLER              PIC X(25) VALUE SPACES.
           05  FILLER              PIC X(25)
               VALUE "Sta. Mesa, Manila".

       01  HEADER-3.
           05  FILLER              PIC X(28) VALUE SPACES.
           05  FILLER              PIC X(20)
               VALUE "STUDENTS REPORT".

       01  HEADER-4.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(10)
               VALUE "STUDENT".
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(10)
               VALUE "STUDENT".
           05  FILLER              PIC X(15) VALUE SPACES.
           05  FILLER              PIC X(10)
               VALUE "AVERAGE".

       01  HEADER-5.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(10)
               VALUE "NUMBER".
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(10)
               VALUE "NAME".
           05  FILLER              PIC X(15) VALUE SPACES.
           05  FILLER              PIC X(10)
               VALUE "GRADE".

      * Detail line for student (indented)
       01  DETAIL-LINE.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  SNO-OUT             PIC X(3).
           05  FILLER              PIC X(17) VALUE SPACES.
           05  SNAME-OUT           PIC X(25).
           05  AVE-OUT             PIC Z.99.


       01  DEPT-LINE.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(20)
               VALUE "Department Name: ".
           05  DEPT-NAME-OUT       PIC X(30).

       01  PASSED-LINE.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(20)
               VALUE "Total Passed: ".
           05  PCTR-OUT            PIC ZZZ9.

       01  FAILED-LINE.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(20)
               VALUE "Total Failed: ".
           05  FCTR-OUT            PIC ZZZ9.

       01  TOTAL-LINE.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(20)
               VALUE "Total Students: ".
           05  SCTR-OUT            PIC ZZZ9.


       01  OVERALL-LINE.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(40)
               VALUE "Overall Total Number of Students: ".
           05  OVSCTR-OUT          PIC ZZZ9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INIT-PARA
           PERFORM READ-PARA
           PERFORM PROCESS-LOOP UNTIL EOF-FLAG = 'Y'
           PERFORM TERMINATION-PARA
           STOP RUN.

       INIT-PARA.
           OPEN INPUT INFILE
           OPEN OUTPUT OUTFILE
           PERFORM WRITE-HEADERS.

       WRITE-HEADERS.
           WRITE REPORT-LINE FROM HEADER-1
           WRITE REPORT-LINE FROM HEADER-2
           WRITE REPORT-LINE FROM HEADER-3
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           WRITE REPORT-LINE FROM HEADER-4
           WRITE REPORT-LINE FROM HEADER-5
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE.

       READ-PARA.
           READ INFILE
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ.

       PROCESS-LOOP.
           IF FIRST-RECORD = 'Y' THEN
               MOVE DEPT-CD TO PREV-DEPT-CD
               MOVE SNO-IN TO PREV-SNO
               MOVE SNAME-IN TO STUDENT-NAME-HOLD
               MOVE 'N' TO FIRST-RECORD
           END-IF

      * Check for department break
           IF DEPT-CD NOT = PREV-DEPT-CD THEN
               PERFORM STUDENT-BREAK
               PERFORM DEPT-BREAK
               MOVE DEPT-CD TO PREV-DEPT-CD
               MOVE SNO-IN TO PREV-SNO
               MOVE SNAME-IN TO STUDENT-NAME-HOLD
               MOVE 0 TO GRADE-TOTAL
               MOVE 0 TO SUBJECT-COUNT
           ELSE
      * Check for student break
               IF SNO-IN NOT = PREV-SNO THEN
                   PERFORM STUDENT-BREAK
                   MOVE SNO-IN TO PREV-SNO
                   MOVE SNAME-IN TO STUDENT-NAME-HOLD
                   MOVE 0 TO GRADE-TOTAL
                   MOVE 0 TO SUBJECT-COUNT
               END-IF
           END-IF

      * Accumulate grades for current student
           ADD GRD TO GRADE-TOTAL
           ADD 1 TO SUBJECT-COUNT

           PERFORM READ-PARA.

       STUDENT-BREAK.
      * Calculate average
           IF SUBJECT-COUNT > 0 THEN
               DIVIDE GRADE-TOTAL BY SUBJECT-COUNT
                   GIVING AVE-CALC ROUNDED

      * Write student detail
               MOVE PREV-SNO TO SNO-OUT
               MOVE STUDENT-NAME-HOLD TO SNAME-OUT
               MOVE AVE-CALC TO AVE-OUT
               WRITE REPORT-LINE FROM DETAIL-LINE

      * Update department counters
               ADD 1 TO SCTR
               ADD 1 TO OVSCTR
               IF AVE-CALC <= 3.00 THEN
                   ADD 1 TO PCTR
               ELSE
                   ADD 1 TO FCTR
               END-IF
           END-IF.

       DEPT-BREAK.
      * Determine department name
           EVALUATE PREV-DEPT-CD
               WHEN "DIT"
                   MOVE "Information Technology" TO DEPT-NAME
               WHEN "DCS"
                   MOVE "Computer Science" TO DEPT-NAME
               WHEN OTHER
                   MOVE "Unknown Department" TO DEPT-NAME
           END-EVALUATE

      * Write department summary
           MOVE DEPT-NAME TO DEPT-NAME-OUT
           WRITE REPORT-LINE FROM DEPT-LINE

           MOVE PCTR TO PCTR-OUT
           WRITE REPORT-LINE FROM PASSED-LINE

           MOVE FCTR TO FCTR-OUT
           WRITE REPORT-LINE FROM FAILED-LINE

           MOVE SCTR TO SCTR-OUT
           WRITE REPORT-LINE FROM TOTAL-LINE

           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE

      * Reset department counters
           MOVE 0 TO PCTR
           MOVE 0 TO FCTR
           MOVE 0 TO SCTR.

       TERMINATION-PARA.
      * Process last student
           IF SUBJECT-COUNT > 0 THEN
               PERFORM STUDENT-BREAK
           END-IF

      * Process last department
           PERFORM DEPT-BREAK

      * Write overall total
           MOVE OVSCTR TO OVSCTR-OUT
           WRITE REPORT-LINE FROM OVERALL-LINE

           CLOSE INFILE
           CLOSE OUTFILE.
