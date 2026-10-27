      ******************************************************************
      * PROGRAM-ID: ACCTMGMT
      * DESCRIPTION: Core Banking Account Management Module
      * DOMAIN: Core Banking & Financial Services
      * VERSION: 1.0.0
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTMGMT.
       AUTHOR. ENTERPRISE-BANKING-SYSTEMS.
       DATE-WRITTEN. 2024-01-15.
       DATE-COMPILED.
      
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZOS.
       OBJECT-COMPUTER. IBM-ZOS.
      
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-MASTER-FILE
               ASSIGN TO ACCTMAST
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NUMBER
               FILE STATUS IS WS-FILE-STATUS.
      
           SELECT TRANSACTION-LOG-FILE
               ASSIGN TO TXNLOG
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-LOG-STATUS.
      
      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
      
       FD  ACCOUNT-MASTER-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 256 CHARACTERS.
      
       01  ACCOUNT-MASTER-RECORD.
           05  ACCT-NUMBER              PIC X(12).
           05  ACCT-HOLDER-NAME         PIC X(40).
           05  ACCT-TYPE                PIC X(02).
               88  SAVINGS-ACCOUNT      VALUE 'SA'.
               88  CHECKING-ACCOUNT     VALUE 'CA'.
               88  FIXED-DEPOSIT        VALUE 'FD'.
           05  ACCT-BALANCE             PIC S9(13)V99 COMP-3.
           05  ACCT-OPEN-DATE           PIC 9(08).
           05  ACCT-LAST-TXN-DATE       PIC 9(08).
           05  ACCT-STATUS              PIC X(01).
               88  ACCT-ACTIVE          VALUE 'A'.
               88  ACCT-DORMANT         VALUE 'D'.
               88  ACCT-CLOSED          VALUE 'C'.
           05  ACCT-BRANCH-CODE         PIC X(06).
           05  ACCT-INTEREST-RATE       PIC 9(02)V9(04).
           05  FILLER                   PIC X(170).
      
       FD  TRANSACTION-LOG-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 128 CHARACTERS.
      
       01  TRANSACTION-LOG-RECORD.
           05  TXN-TIMESTAMP            PIC X(26).
           05  TXN-ACCOUNT-NUM          PIC X(12).
           05  TXN-TYPE                 PIC X(03).
           05  TXN-AMOUNT               PIC S9(13)V99 COMP-3.
           05  TXN-STATUS               PIC X(02).
           05  TXN-OPERATOR-ID          PIC X(08).
           05  FILLER                   PIC X(69).
      
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
       01  WS-FILE-STATUS               PIC X(02).
           88  WS-FILE-OK               VALUE '00'.
           88  WS-FILE-EOF              VALUE '10'.
           88  WS-FILE-NOT-FOUND        VALUE '23'.
      
       01  WS-LOG-STATUS                PIC X(02).
      
       01  WS-WORKING-FIELDS.
           05  WS-CURRENT-DATE          PIC 9(08).
           05  WS-CURRENT-TIME          PIC 9(06).
           05  WS-RETURN-CODE           PIC S9(04) COMP VALUE ZERO.
           05  WS-ERROR-MSG             PIC X(80) VALUE SPACES.
      
       01  WS-ACCOUNT-WORK-AREA.
           05  WS-NEW-BALANCE           PIC S9(13)V99 COMP-3.
           05  WS-MIN-BALANCE           PIC S9(13)V99 COMP-3
                                        VALUE 100.00.
           05  WS-DAILY-LIMIT           PIC S9(13)V99 COMP-3
                                        VALUE 50000.00.
      
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-ACCOUNTS
           PERFORM 9000-TERMINATE
           STOP RUN.
      
      ******************************************************************
       1000-INITIALIZE.
      ******************************************************************
           OPEN I-O ACCOUNT-MASTER-FILE
           IF NOT WS-FILE-OK
               MOVE 'ERROR OPENING ACCOUNT MASTER FILE'
                   TO WS-ERROR-MSG
               MOVE 12 TO WS-RETURN-CODE
               PERFORM 9000-TERMINATE
           END-IF
      
           OPEN OUTPUT TRANSACTION-LOG-FILE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME.
      
      ******************************************************************
       2000-PROCESS-ACCOUNTS.
      ******************************************************************
           PERFORM 2100-READ-ACCOUNT
           PERFORM UNTIL WS-FILE-EOF
               PERFORM 2200-VALIDATE-ACCOUNT
               PERFORM 2100-READ-ACCOUNT
           END-PERFORM.
      
      ******************************************************************
       2100-READ-ACCOUNT.
      ******************************************************************
           READ ACCOUNT-MASTER-FILE NEXT RECORD
               AT END SET WS-FILE-EOF TO TRUE
           END-READ.
      
      ******************************************************************
       2200-VALIDATE-ACCOUNT.
      ******************************************************************
           IF ACCT-ACTIVE
               IF ACCT-BALANCE < WS-MIN-BALANCE
                   PERFORM 2300-FLAG-LOW-BALANCE
               END-IF
           END-IF.
      
      ******************************************************************
       2300-FLAG-LOW-BALANCE.
      ******************************************************************
           MOVE WS-CURRENT-DATE TO ACCT-LAST-TXN-DATE
           REWRITE ACCOUNT-MASTER-RECORD.
      
      ******************************************************************
       9000-TERMINATE.
      ******************************************************************
           CLOSE ACCOUNT-MASTER-FILE
           CLOSE TRANSACTION-LOG-FILE
           MOVE WS-RETURN-CODE TO RETURN-CODE.
      
