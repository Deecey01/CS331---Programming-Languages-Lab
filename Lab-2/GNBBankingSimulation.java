import java.io.FileWriter;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.ReentrantLock;

// Represents an individual customer account with thread-safe financial operations
class Customer {
    String accountNumber;
    private double balance;
    // ReentrantLock ensures thread-safe access to customer account operations
    ReentrantLock lock = new ReentrantLock(); 

    // Constructor to initialize customer account
    Customer(String accountNumber, double balance) {
        this.accountNumber = accountNumber;
        this.balance = balance;
    }

    // Thread-safe deposit method with lock to prevent concurrent modifications
    void deposit(double amount) {
        lock.lock();
        try {
            balance += amount;
        } finally {
            lock.unlock(); // Ensures lock is released even if an exception occurs
        }
    }

    // Thread-safe withdrawal method with additional balance check
    void withdraw(double amount) {
        lock.lock();
        try {
            // Prevent overdraft by checking balance before withdrawal
            if (balance >= amount) {
                balance -= amount;
            }
        } finally {
            lock.unlock();
        }
    }

    // Thread-safe balance retrieval method
    double getBalance() {
        lock.lock();
        try {
            return balance;
        } finally {
            lock.unlock();
        }
    }

    // Thread-safe transfer method with deadlock prevention
    void transfer(Customer recipient, double amount) {
        // Ensure consistent locking order to prevent potential deadlocks
        ReentrantLock firstLock = this.lock;
        ReentrantLock secondLock = recipient.lock;

        // Deterministic lock ordering based on account number comparison
        if (this.accountNumber.compareTo(recipient.accountNumber) < 0) {
            firstLock.lock();
            secondLock.lock();
        } else {
            secondLock.lock();
            firstLock.lock();
        }

        try {
            // Perform transfer only if sufficient balance exists
            if (this.balance >= amount) {
                this.balance -= amount;
                recipient.balance += amount;
            }
        } finally {
            // Ensure locks are released in reverse order of acquisition
            firstLock.unlock();
            secondLock.unlock();
        }
    }
}

// Represents a bank branch with thread-safe customer management
class Branch {
    String branchId;
    // ConcurrentHashMap ensures thread-safe map operations
    ConcurrentHashMap<String, Customer> customers;
    // CopyOnWriteArrayList provides thread-safe list operations with O(1) random access
    CopyOnWriteArrayList<String> accountNumbers;
    // Branch-level lock for atomic operations on branch resources
    ReentrantLock branchLock = new ReentrantLock(); 

    // Constructor initializes branch with thread-safe collections
    Branch(String branchId) {
        this.branchId = branchId;
        this.customers = new ConcurrentHashMap<>();
        this.accountNumbers = new CopyOnWriteArrayList<>();
    }

    // Thread-safe method to add a new customer to the branch
    void addCustomer(Customer customer) {
        branchLock.lock();
        try {
            customers.put(customer.accountNumber, customer);
            accountNumbers.add(customer.accountNumber);
        } finally {
            branchLock.unlock();
        }
    }

    // Thread-safe method to remove a customer from the branch
    void removeCustomer(String accountNumber) {
        branchLock.lock();
        try {
            customers.remove(accountNumber);
            accountNumbers.remove(accountNumber);
        } finally {
            branchLock.unlock();
        }
    }

    // Retrieves a customer by account number
    Customer getCustomer(String accountNumber) {
        return customers.get(accountNumber);
    }

    // Thread-safe method to get a random account number from the branch
    String getRandomAccountNumber(Random random) {
        branchLock.lock();
        try {
            if (accountNumbers.isEmpty()) return null;
            return accountNumbers.get(random.nextInt(accountNumbers.size()));
        } finally {
            branchLock.unlock();
        }
    }
}

// Represents the entire bank with multiple branches and global operations
class GNB {
    // Thread-safe map to store bank branches
    ConcurrentHashMap<String, Branch> branches;
    // Atomic counter for generating unique, sequential account numbers
    AtomicLong accountCounter = new AtomicLong(1);
    ReentrantLock transferLock = new ReentrantLock(); // Lock for inter-branch transfers

    // Constructor initializes 10 branches
    GNB() {
        branches = new ConcurrentHashMap<>();
        for (int i = 0; i < 10; i++) {
            String branchId = "Branch" + i;
            branches.put(branchId, new Branch(branchId));
        }
    }

    // Determine the branch based on the first digit of the account number
    Branch getBranch(String accountNumber) {
        String branchId = "Branch" + accountNumber.charAt(0);
        return branches.get(branchId);
    }

    // Transfer a customer between branches
    void transferCustomer(String accountNumber, String newBranchId) {
        transferLock.lock();
        try {
            Branch sourceBranch = getBranch(accountNumber);
            Branch destinationBranch = branches.get(newBranchId);
            if (sourceBranch != null && destinationBranch != null) {
                Customer customer = sourceBranch.getCustomer(accountNumber);
                if (customer != null) {
                    sourceBranch.removeCustomer(accountNumber);
                    destinationBranch.addCustomer(customer);
                }
            }
        } finally {
            transferLock.unlock();
        }
    }

    // Generate a unique account number using atomic increment
    String generateAccountNumber() {
        return String.valueOf(accountCounter.getAndIncrement());
    }
}

// Represents a thread that simulates various banking transactions
class Updater implements Runnable {
    // Predefined transaction types with corresponding probabilities
    private static final String[] REQUEST_TYPES = {
            "BalanceCheck", "CashDeposit", "CashWithdrawal", "MoneyTransfer", 
            "AddCustomer", "DeleteCustomer", "TransferCustomer"
    };
    private static final double[] PROBABILITIES = {0.3, 0.23, 0.23, 0.23, 0.003, 0.003, 0.004};

    private final GNB gnb;
    private final String branchId;
    private final Random random;
    private final FileWriter logWriter;
    private final boolean enableLogging; // Flag to control transaction logging

    // Constructor initializes updater with bank, branch, and logging details
    Updater(GNB gnb, String branchId, FileWriter logWriter, boolean enableLogging) {
        this.gnb = gnb;
        this.branchId = branchId;
        this.random = new Random();
        this.logWriter = logWriter;
        this.enableLogging = enableLogging;
    }

    // Thread-safe logging method with synchronization on logWriter
    private void logTransaction(String message) {
        if (enableLogging) {
            synchronized (logWriter) {
                try {
                    logWriter.write(message + "\n");
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    // Main transaction simulation method
    @Override
    public void run() {
        // Simulate 10^6 transactions per updater thread
        for (int i = 0; i < 1000000; i++) {
            // Randomly select transaction type based on predefined probabilities
            double rand = random.nextDouble();
            double cumulativeProbability = 0.0;
            String requestType = "";

            for (int j = 0; j < PROBABILITIES.length; j++) {
                cumulativeProbability += PROBABILITIES[j];
                if (rand < cumulativeProbability) {
                    requestType = REQUEST_TYPES[j];
                    break;
                }
            }

            // Retrieve current branch
            Branch branch = gnb.branches.get(branchId);
            
            // Perform transaction based on selected request type
            switch (requestType) {
                case "BalanceCheck":
                    String accountNumber = branch.getRandomAccountNumber(random);
                    if (accountNumber != null) {
                        Customer customer = branch.getCustomer(accountNumber);
                        if (customer != null) {
                            double balance = customer.getBalance();
                            logTransaction("BalanceCheck: Account " + accountNumber + " has balance " + balance);
                        }
                    }
                    break;

                case "CashDeposit":
                    accountNumber = branch.getRandomAccountNumber(random);
                    if (accountNumber != null) {
                        Customer customer = branch.getCustomer(accountNumber);
                        if (customer != null) {
                            double depositAmount = random.nextDouble() * 1000;
                            customer.deposit(depositAmount);
                            logTransaction("CashDeposit: Deposited " + depositAmount + " to Account " + accountNumber + ". New Balance: " + customer.getBalance());
                        }
                    }
                    break;

                case "CashWithdrawal":
                    accountNumber = branch.getRandomAccountNumber(random);
                    if (accountNumber != null) {
                        Customer customer = branch.getCustomer(accountNumber);
                        if (customer != null) {
                            double withdrawalAmount = random.nextDouble() * customer.getBalance();
                            customer.withdraw(withdrawalAmount);
                            logTransaction("CashWithdrawal: Withdrawn " + withdrawalAmount + " from Account " + accountNumber + ". New Balance: " + customer.getBalance());
                        }
                    }
                    break;

                case "MoneyTransfer":
                    accountNumber = branch.getRandomAccountNumber(random);
                    if (accountNumber != null) {
                        Customer sourceCustomer = branch.getCustomer(accountNumber);
                        if (sourceCustomer != null) {
                            Branch destBranch = gnb.branches.get("Branch" + random.nextInt(10));
                            String destAccount = destBranch.getRandomAccountNumber(random);
                            if (destAccount != null) {
                                Customer destCustomer = destBranch.getCustomer(destAccount);
                                if (destCustomer != null) {
                                    double transferAmount = random.nextDouble() * sourceCustomer.getBalance();
                                    sourceCustomer.transfer(destCustomer, transferAmount);
                                    logTransaction("MoneyTransfer: Transferred " + transferAmount + " from " + accountNumber + " to " + destAccount + ". Source New Balance: " + sourceCustomer.getBalance() + ", Destination New Balance: " + destCustomer.getBalance());
                                }
                            }
                        }
                    }
                    break;

                case "AddCustomer":
                    String newAccountNumber = gnb.generateAccountNumber();
                    // String newAccountNumber = branchId.charAt(6) + String.format("%09d", random.nextInt(1000000000));
                    double initialBalance = random.nextDouble() * 10000;
                    Customer newCustomer = new Customer(newAccountNumber, initialBalance);
                    branch.addCustomer(newCustomer);
                    logTransaction("AddCustomer: Added Account " + newAccountNumber + " with balance " + initialBalance);
                    break;

                case "DeleteCustomer":
                    accountNumber = branch.getRandomAccountNumber(random);
                    if (accountNumber != null) {
                        branch.removeCustomer(accountNumber);
                        logTransaction("DeleteCustomer: Deleted Account " + accountNumber);
                    }
                    break;

                case "TransferCustomer":
                    accountNumber = branch.getRandomAccountNumber(random);
                    if (accountNumber != null) {
                        String newBranchId = "Branch" + random.nextInt(10);
                        gnb.transferCustomer(accountNumber, newBranchId);
                        logTransaction("TransferCustomer: Transferred Account " + accountNumber + " to " + newBranchId);
                    }
                    break;
            }
        }
    }
}

// Main class to run the banking simulation
public class GNBBankingSimulation {
    public static void main(String[] args) {
        // Create bank instance
        GNB gnb = new GNB();
        // Create thread pool for concurrent transaction processing
        ExecutorService executor = Executors.newFixedThreadPool(100);
        // Record start time for performance measurement
        long startTime = System.currentTimeMillis();

        // Initialize 10^4 customers per branch with random initial balances
        // Random random = new Random();
        for (int i = 0; i < 10; i++) {
            Branch branch = gnb.branches.get("Branch" + i);
            for (int j = 0; j < 10000; j++) {
                String accountNumber = gnb.generateAccountNumber();
                // String accountNumber = i + String.format("%09d", random.nextInt(1000000000));
                branch.addCustomer(new Customer(accountNumber, new Random().nextDouble() * 10000));
            }
        }

        // Logging configuration
        boolean enableLogging = false; // Set to `true` to enable transaction logging

        try (FileWriter logWriter = enableLogging ? new FileWriter("gnb_transactions.log") : null) {
            // Create and submit updater threads for each branch
            for (int i = 0; i < 10; i++) {
                String branchId = "Branch" + i;
                for (int j = 0; j < 10; j++) {
                    executor.submit(new Updater(gnb, branchId, logWriter, enableLogging));
                }
            }

            // Shutdown executor and wait for all threads to complete
            executor.shutdown();
            executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
        } finally {
            // Calculate and print total execution time
            long endTime = System.currentTimeMillis();
            System.out.println("Execution Time: " + (endTime - startTime) + " ms");
        }
    }
}