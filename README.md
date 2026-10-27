# Core Banking COBOL System - Sample Repository

## Business Domain
**Core Banking & Financial Services**

## Overview
This repository contains a sanitized sample of a proprietary COBOL-based core banking system demonstrating:

- Account Management (ACCTMGMT)
- Transaction Processing (TXNPROC)
- Shared Copybooks
- JCL Batch Processing

## Repository Structure
```
├── src/
│   ├── ACCTMGMT.cbl    # Account management module (160 LOC)
│   └── TXNPROC.cbl     # Transaction processing module (302 LOC)
├── copybook/
│   └── ACCTCPY.cpy     # Shared data definitions (75 LOC)
├── jcl/
│   └── ACCTJOB.jcl     # Batch job control (29 LOC)
└── README.md
```

## Code Statistics
| Component | Lines of Code | Description |
|-----------|---------------|-------------|
| ACCTMGMT.cbl | 160 | Account lifecycle management |
| TXNPROC.cbl | 302 | Transaction processing engine |
| ACCTCPY.cpy | 75 | Shared copybook definitions |
| ACCTJOB.jcl | 29 | JCL batch orchestration |
| **Total** | **566** | - |

## COBOL Division Structure
Each program follows standard IBM COBOL structure:
1. **IDENTIFICATION DIVISION** - Program metadata
2. **ENVIRONMENT DIVISION** - File control and configuration
3. **DATA DIVISION** - File Section, Working-Storage Section
4. **PROCEDURE DIVISION** - Business logic paragraphs

## Features Demonstrated
- Indexed VSAM file access (ORGANIZATION IS INDEXED)
- COMP-3 packed decimal fields for currency
- 88-level condition names
- EVALUATE statements for transaction routing
- Comprehensive audit trail logging
- Business rule enforcement (limits, balances)

## Compliance Notes
- **IP Status**: Proprietary - Clean of open-source licenses
- **License Contamination**: None (zero GPL/MIT/Apache)
- **Employer NDA**: Not applicable

## Git Commit History
Repository includes full commit history showing incremental development.
