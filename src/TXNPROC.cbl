      ******************************************************************
      * PROGRAM-ID: TXNPROC
      * DESCRIPTION: Core Banking Transaction Processing Module
      * DOMAIN: Core Banking & Financial Services
      * VERSION: 1.0.0
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXNPROC.
       AUTHOR. ENTERPRISE-BANKING-SYSTEMS.
       DATE-WRITTEN. 2024-01-20.
       DATE-COMPILED.
      
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZOS.
       OBJECT-COMPUTER. IBM-ZOS.
      
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-INPUT-FILE
               ASSIGN TO TXNINPUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-TXN-STATUS.
      
           SELECT ACCOUNT-MASTER-FILE
               ASSIGN TO ACCTMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS AM-ACCT-NUMBER
               FILE STATUS IS WS-ACCT-STATUS.
      
           SELECT AUDIT-TRAIL-FILE
               ASSIGN TO AUDTTRAIL
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-AUDIT-STATUS.
      
      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
      
       FD  TRANSACTION-INPUT-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 100 CHARACTERS.
      
       01  TRANSACTION-INPUT-RECORD.
           05  TI-TXN-CODE              PIC X(03).
               88  TI-DEPOSIT           VALUE 'DEP'.
               88  TI-WITHDRAWAL        VALUE 'WDR'.
               88  TI-TRANSFER          VALUE 'TRF'.
               88  TI-INQUIRY           VALUE 'INQ'.
           05  TI-ACCOUNT-NUM           PIC X(12).
           05  TI-AMOUNT                PIC S9(11)V99 COMP-3.
           05  TI-TARGET-ACCOUNT        PIC X(12).
           05  TI-BRANCH-CODE           PIC X(06).
           05  TI-TELLER-ID             PIC X(08).
           05  TI-TXN-DATE              PIC 9(08).
           05  TI-TXN-TIME              PIC 9(06).
           05  FILLER                   PIC X(38).
      
       FD  ACCOUNT-MASTER-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 256 CHARACTERS.
      
       01  ACCOUNT-MASTER-RECORD.
           05  AM-ACCT-NUMBER           PIC X(12).
           05  AM-HOLDER-NAME           PIC X(40).
           05  AM-ACCT-TYPE             PIC X(02).
           05  AM-BALANCE               PIC S9(13)V99 COMP-3.
           05  AM-OPEN-DATE             PIC 9(08).
           05  AM-LAST-TXN-DATE         PIC 9(08).
           05  AM-STATUS                PIC X(01).
               88  AM-ACTIVE            VALUE 'A'.
           05  AM-BRANCH-CODE           PIC X(06).
           05  AM-INTEREST-RATE         PIC 9(02)V9(04).
           05  FILLER                   PIC X(170).
      
       FD  AUDIT-TRAIL-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 200 CHARACTERS.
      
       01  AUDIT-TRAIL-RECORD.
           05  AT-TIMESTAMP             PIC X(26).
           05  AT-TXN-CODE              PIC X(03).
           05  AT-ACCOUNT-NUM           PIC X(12).
           05  AT-AMOUNT                PIC S9(13)V99 COMP-3.
           05  AT-PREV-BALANCE          PIC S9(13)V99 COMP-3.
           05  AT-NEW-BALANCE           PIC S9(13)V99 COMP-3.
           05  AT-STATUS-CODE           PIC X(02).
           05  AT-TELLER-ID             PIC X(08).
           05  AT-ERROR-DESC            PIC X(50).
           05  FILLER                   PIC X(75).
      
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
       01  WS-FILE-STATUSES.
           05  WS-TXN-STATUS            PIC X(02).
               88  WS-TXN-OK            VALUE '00'.
               88  WS-TXN-EOF           VALUE '10'.
           05  WS-ACCT-STATUS           PIC X(02).
               88  WS-ACCT-OK           VALUE '00'.
               88  WS-ACCT-NOT-FOUND    VALUE '23'.
           05  WS-AUDIT-STATUS          PIC X(02).
      
       01  WS-COUNTERS.
           05  WS-TXN-READ-COUNT        PIC 9(09) VALUE ZERO.
           05  WS-TXN-PROC-COUNT        PIC 9(09) VALUE ZERO.
           05  WS-TXN-ERROR-COUNT       PIC 9(09) VALUE ZERO.
           05  WS-DEPOSIT-TOTAL         PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-WITHDRAWAL-TOTAL      PIC S9(15)V99 COMP-3 VALUE ZERO.
      
       01  WS-WORKING-FIELDS.
           05  WS-CURRENT-TIMESTAMP     PIC X(26).
           05  WS-NEW-BALANCE           PIC S9(13)V99 COMP-3.
           05  WS-PREV-BALANCE          PIC S9(13)V99 COMP-3.
           05  WS-RETURN-CODE           PIC S9(04) COMP VALUE ZERO.
      
       01  WS-BUSINESS-RULES.
           05  WS-MIN-BALANCE           PIC S9(13)V99 COMP-3
                                        VALUE 100.00.
           05  WS-DAILY-WDR-LIMIT       PIC S9(13)V99 COMP-3
                                        VALUE 50000.00.
           05  WS-MAX-DEPOSIT           PIC S9(13)V99 COMP-3
                                        VALUE 1000000.00.
      
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-TRANSACTIONS
           PERFORM 3000-PRINT-SUMMARY
           PERFORM 9000-TERMINATE
           STOP RUN.
      
      ******************************************************************
       1000-INITIALIZE.
      ******************************************************************
           OPEN INPUT  TRANSACTION-INPUT-FILE
           OPEN I-O    ACCOUNT-MASTER-FILE
           OPEN OUTPUT AUDIT-TRAIL-FILE
      
           IF NOT WS-TXN-OK OR NOT WS-ACCT-OK
               MOVE 12 TO WS-RETURN-CODE
               PERFORM 9000-TERMINATE
           END-IF
      
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TIMESTAMP.
      
      ******************************************************************
       2000-PROCESS-TRANSACTIONS.
      ******************************************************************
           PERFORM 2100-READ-TRANSACTION
           PERFORM UNTIL WS-TXN-EOF
               ADD 1 TO WS-TXN-READ-COUNT
               PERFORM 2200-VALIDATE-TRANSACTION
               PERFORM 2100-READ-TRANSACTION
           END-PERFORM.
      
      ******************************************************************
       2100-READ-TRANSACTION.
      ******************************************************************
           READ TRANSACTION-INPUT-FILE
               AT END SET WS-TXN-EOF TO TRUE
           END-READ.
      
      ******************************************************************
       2200-VALIDATE-TRANSACTION.
      ******************************************************************
           MOVE TI-ACCOUNT-NUM TO AM-ACCT-NUMBER
           READ ACCOUNT-MASTER-FILE
      
           IF WS-ACCT-NOT-FOUND
               MOVE 'ACCOUNT NOT FOUND' TO AT-ERROR-DESC
               PERFORM 2900-LOG-ERROR
           ELSE
               IF NOT AM-ACTIVE
                   MOVE 'ACCOUNT INACTIVE' TO AT-ERROR-DESC
                   PERFORM 2900-LOG-ERROR
               ELSE
                   PERFORM 2300-EXECUTE-TRANSACTION
               END-IF
           END-IF.
      
      ******************************************************************
       2300-EXECUTE-TRANSACTION.
      ******************************************************************
           MOVE AM-BALANCE TO WS-PREV-BALANCE
      
           EVALUATE TRUE
               WHEN TI-DEPOSIT
                   PERFORM 2400-PROCESS-DEPOSIT
               WHEN TI-WITHDRAWAL
                   PERFORM 2500-PROCESS-WITHDRAWAL
               WHEN TI-TRANSFER
                   PERFORM 2600-PROCESS-TRANSFER
               WHEN TI-INQUIRY
                   PERFORM 2700-PROCESS-INQUIRY
               WHEN OTHER
                   MOVE 'INVALID TXN CODE' TO AT-ERROR-DESC
                   PERFORM 2900-LOG-ERROR
           END-EVALUATE.
      
      ******************************************************************
       2400-PROCESS-DEPOSIT.
      ******************************************************************
           IF TI-AMOUNT > WS-MAX-DEPOSIT
               MOVE 'EXCEEDS DEPOSIT LIMIT' TO AT-ERROR-DESC
               PERFORM 2900-LOG-ERROR
           ELSE
               ADD TI-AMOUNT TO AM-BALANCE
               ADD TI-AMOUNT TO WS-DEPOSIT-TOTAL
               MOVE TI-TXN-DATE TO AM-LAST-TXN-DATE
               REWRITE ACCOUNT-MASTER-RECORD
               ADD 1 TO WS-TXN-PROC-COUNT
               PERFORM 2800-LOG-SUCCESS
           END-IF.
      
      ******************************************************************
       2500-PROCESS-WITHDRAWAL.
      ******************************************************************
           COMPUTE WS-NEW-BALANCE = AM-BALANCE - TI-AMOUNT
      
           IF WS-NEW-BALANCE < WS-MIN-BALANCE
               MOVE 'INSUFFICIENT FUNDS' TO AT-ERROR-DESC
               PERFORM 2900-LOG-ERROR
           ELSE
               IF TI-AMOUNT > WS-DAILY-WDR-LIMIT
                   MOVE 'EXCEEDS DAILY LIMIT' TO AT-ERROR-DESC
                   PERFORM 2900-LOG-ERROR
               ELSE
                   SUBTRACT TI-AMOUNT FROM AM-BALANCE
                   ADD TI-AMOUNT TO WS-WITHDRAWAL-TOTAL
                   MOVE TI-TXN-DATE TO AM-LAST-TXN-DATE
                   REWRITE ACCOUNT-MASTER-RECORD
                   ADD 1 TO WS-TXN-PROC-COUNT
                   PERFORM 2800-LOG-SUCCESS
               END-IF
           END-IF.
      
      ******************************************************************
       2600-PROCESS-TRANSFER.
      ******************************************************************
           CONTINUE.
      
      ******************************************************************
       2700-PROCESS-INQUIRY.
      ******************************************************************
           ADD 1 TO WS-TXN-PROC-COUNT
           PERFORM 2800-LOG-SUCCESS.
      
      ******************************************************************
       2800-LOG-SUCCESS.
      ******************************************************************
           MOVE WS-CURRENT-TIMESTAMP TO AT-TIMESTAMP
           MOVE TI-TXN-CODE          TO AT-TXN-CODE
           MOVE TI-ACCOUNT-NUM       TO AT-ACCOUNT-NUM
           MOVE TI-AMOUNT            TO AT-AMOUNT
           MOVE WS-PREV-BALANCE      TO AT-PREV-BALANCE
           MOVE AM-BALANCE           TO AT-NEW-BALANCE
           MOVE 'OK'                 TO AT-STATUS-CODE
           MOVE TI-TELLER-ID         TO AT-TELLER-ID
           MOVE SPACES               TO AT-ERROR-DESC
           WRITE AUDIT-TRAIL-RECORD.
      
      ******************************************************************
       2900-LOG-ERROR.
      ******************************************************************
           MOVE WS-CURRENT-TIMESTAMP TO AT-TIMESTAMP
           MOVE TI-TXN-CODE          TO AT-TXN-CODE
           MOVE TI-ACCOUNT-NUM       TO AT-ACCOUNT-NUM
           MOVE TI-AMOUNT            TO AT-AMOUNT
           MOVE AM-BALANCE           TO AT-PREV-BALANCE
           MOVE AM-BALANCE           TO AT-NEW-BALANCE
           MOVE 'ER'                 TO AT-STATUS-CODE
           MOVE TI-TELLER-ID         TO AT-TELLER-ID
           WRITE AUDIT-TRAIL-RECORD
           ADD 1 TO WS-TXN-ERROR-COUNT.
      
      ******************************************************************
       3000-PRINT-SUMMARY.
      ******************************************************************
           DISPLAY '========================================='
           DISPLAY 'TRANSACTION PROCESSING SUMMARY'
           DISPLAY '========================================='
           DISPLAY 'TRANSACTIONS READ:      ' WS-TXN-READ-COUNT
           DISPLAY 'TRANSACTIONS PROCESSED: ' WS-TXN-PROC-COUNT
           DISPLAY 'TRANSACTIONS IN ERROR:  ' WS-TXN-ERROR-COUNT
           DISPLAY '========================================='.
      
      ******************************************************************
       9000-TERMINATE.
      ******************************************************************
           CLOSE TRANSACTION-INPUT-FILE
           CLOSE ACCOUNT-MASTER-FILE
           CLOSE AUDIT-TRAIL-FILE
           MOVE WS-RETURN-CODE TO RETURN-CODE.
      
