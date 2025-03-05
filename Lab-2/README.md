# GNB Banking Simulation

## Overview
A multithreaded Java banking system simulation for Guwahati National Bank (GNB) modeling concurrent financial transactions across 10 branches.

## Features
- Simulates up to 10^6 transactions per updater
- 10 bank branches with 10^4 initial customers per branch
- Thread-safe transaction operations
- Incorporated locking mechanism at 3 different levels :
  - Customer-level locking
  - Branch-level locking
  - Transfer-level locking
- Logging mechanism available (if `enabled`, would store in `gnb_transactions.log` but would take longer durations because of I/O operations)
```bash
    // Logging configuration
    boolean enableLogging = false; // Set to `true` to enable transaction logging
```
- Supports:
  - Balance checks
  - Cash deposits/withdrawals
  - Money transfers
  - Customer account management

## Transaction Probabilities
- Balance Check: 30%
- Cash Deposit: 23%
- Cash Withdrawal: 23%
- Money Transfer: 23%
- Add Customer: 0.3%
- Delete Customer: 0.3%
- Transfer Customer: 0.4%

## Performance
- Concurrent thread execution
- Optimized locking mechanisms
- Execution time measured and reported

## Running the Simulation
```bash
javac GNBBankingSimulation.java
java GNBBankingSimulation
```

## Time taken
- If all the necessary locks are incorporated then the code is taking around `35 seconds` to simulate completely.
- Removing less important locking mechanism will further decrease the time taken.
- Without locking mechanism, the code is executing completely within 10 seconds but with potential issues.

## Key Components
- `Customer`: Individual account management
- `Branch`: Branch-level operations
- `GNB`: Overall bank management
- `Updater`: Transaction simulation thread
