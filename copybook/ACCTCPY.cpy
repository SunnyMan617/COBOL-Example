      ******************************************************************
      * COPYBOOK: ACCTCPY
      * DESCRIPTION: Shared Account Data Structures for Core Banking
      * USED BY: ACCTMGMT, TXNPROC, RPTGEN, INTCALC
      ******************************************************************
      
      ******************************************************************
      * ACCOUNT TYPE DEFINITIONS
      ******************************************************************
       01  WS-ACCOUNT-TYPES.
           05  WS-ACCT-TYPE-SA          PIC X(02) VALUE 'SA'.
           05  WS-ACCT-TYPE-CA          PIC X(02) VALUE 'CA'.
           05  WS-ACCT-TYPE-FD          PIC X(02) VALUE 'FD'.
           05  WS-ACCT-TYPE-RD          PIC X(02) VALUE 'RD'.
           05  WS-ACCT-TYPE-OD          PIC X(02) VALUE 'OD'.
      
      ******************************************************************
      * STANDARD INTEREST RATES (ANNUAL PERCENTAGE)
      ******************************************************************
       01  WS-INTEREST-RATES.
           05  WS-RATE-SAVINGS          PIC 9(02)V9(04) VALUE 03.5000.
           05  WS-RATE-CHECKING         PIC 9(02)V9(04) VALUE 01.0000.
           05  WS-RATE-FIXED-1YR        PIC 9(02)V9(04) VALUE 06.2500.
           05  WS-RATE-FIXED-3YR        PIC 9(02)V9(04) VALUE 07.0000.
           05  WS-RATE-FIXED-5YR        PIC 9(02)V9(04) VALUE 07.5000.
      
      ******************************************************************
      * TRANSACTION LIMITS BY ACCOUNT TYPE
      ******************************************************************
       01  WS-TXN-LIMITS.
           05  WS-LIMIT-SAVINGS.
               10  WS-SAV-DAILY-WDR     PIC S9(11)V99 COMP-3
                                        VALUE 25000.00.
               10  WS-SAV-DAILY-DEP     PIC S9(11)V99 COMP-3
                                        VALUE 500000.00.
               10  WS-SAV-MIN-BAL       PIC S9(11)V99 COMP-3
                                        VALUE 500.00.
           05  WS-LIMIT-CHECKING.
               10  WS-CHK-DAILY-WDR     PIC S9(11)V99 COMP-3
                                        VALUE 100000.00.
               10  WS-CHK-DAILY-DEP     PIC S9(11)V99 COMP-3
                                        VALUE 1000000.00.
               10  WS-CHK-MIN-BAL       PIC S9(11)V99 COMP-3
                                        VALUE 1000.00.
      
      ******************************************************************
      * STATUS CODES AND FLAGS
      ******************************************************************
       01  WS-STATUS-CODES.
           05  WS-STAT-ACTIVE           PIC X(01) VALUE 'A'.
           05  WS-STAT-DORMANT          PIC X(01) VALUE 'D'.
           05  WS-STAT-CLOSED           PIC X(01) VALUE 'C'.
           05  WS-STAT-FROZEN           PIC X(01) VALUE 'F'.
           05  WS-STAT-UNDER-REVIEW     PIC X(01) VALUE 'R'.
      
       01  WS-TXN-RESULT-CODES.
           05  WS-TXN-SUCCESS           PIC X(02) VALUE 'OK'.
           05  WS-TXN-INSUFFICIENT      PIC X(02) VALUE 'IF'.
           05  WS-TXN-LIMIT-EXCEED      PIC X(02) VALUE 'LE'.
           05  WS-TXN-INVALID-ACCT      PIC X(02) VALUE 'IA'.
           05  WS-TXN-SYSTEM-ERROR      PIC X(02) VALUE 'SE'.
      
      ******************************************************************
      * BRANCH CODE STRUCTURE
      ******************************************************************
       01  WS-BRANCH-RECORD.
           05  WS-BRANCH-CODE           PIC X(06).
           05  WS-BRANCH-NAME           PIC X(30).
           05  WS-BRANCH-REGION         PIC X(02).
               88  WS-REGION-NORTH      VALUE 'NO'.
               88  WS-REGION-SOUTH      VALUE 'SO'.
               88  WS-REGION-EAST       VALUE 'EA'.
               88  WS-REGION-WEST       VALUE 'WE'.
               88  WS-REGION-CENTRAL    VALUE 'CE'.
      
