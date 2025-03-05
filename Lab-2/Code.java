// // BankSimulation.java
// import java.util.*;
// import java.util.concurrent.locks.ReentrantReadWriteLock;
// import java.util.concurrent.*;
// import java.util.concurrent.atomic.AtomicInteger;
// import java.text.SimpleDateFormat;

// class Customer {
//     private final String accountNumber;
//     private double balance;
//     private final int branchId;

//     public Customer(String accountNumber, double balance) {
//         this.accountNumber = accountNumber;
//         this.balance = balance;
//         this.branchId = Integer.parseInt(accountNumber.substring(0, 1));
//     }

//     public synchronized String getAccountNumber() {
//         return accountNumber;
//     }

//     public synchronized double getBalance() {
//         return balance;
//     }

//     public synchronized void setBalance(double balance) {
//         this.balance = balance;
//     }

//     public synchronized int getBranchId() {
//         return branchId;
//     }
// }

// class Branch {
//     private final ConcurrentHashMap<String, Customer> customers;
//     private final int branchId;
//     private final ReentrantReadWriteLock lock;

//     public Branch(int branchId) {
//         this.branchId = branchId;
//         this.customers = new ConcurrentHashMap<>();
//         this.lock = new ReentrantReadWriteLock();
//     }

//     public boolean addCustomer(Customer customer) {
//         lock.writeLock().lock();
//         try {
//             return customers.putIfAbsent(customer.getAccountNumber(), customer) == null;
//         } finally {
//             lock.writeLock().unlock();
//         }
//     }

//     public Customer removeCustomer(String accountNumber) {
//         lock.writeLock().lock();
//         try {
//             return customers.remove(accountNumber);
//         } finally {
//             lock.writeLock().unlock();
//         }
//     }

//     public Customer getCustomer(String accountNumber) {
//         lock.readLock().lock();
//         try {
//             return customers.get(accountNumber);
//         } finally {
//             lock.readLock().unlock();
//         }
//     }

//     public Collection<Customer> getAllCustomers() {
//         lock.readLock().lock();
//         try {
//             return new ArrayList<>(customers.values());
//         } finally {
//             lock.readLock().unlock();
//         }
//     }
// }

// class Bank {
//     private final Branch[] branches;
//     private final AtomicInteger customerCounter;
//     private static final int NUM_BRANCHES = 10;
//     private final SimpleDateFormat dateFormat;

//     public Bank() {
//         this.branches = new Branch[NUM_BRANCHES];
//         for (int i = 0; i < NUM_BRANCHES; i++) {
//             this.branches[i] = new Branch(i);
//         }
//         this.customerCounter = new AtomicInteger(0);
//         this.dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
//         initializeCustomers();
//     }

//     private void initializeCustomers() {
//         Random random = new Random();
//         System.out.println("Initializing customers for Guwahati National Bank (GNB)...");
//         for (int branchId = 0; branchId < NUM_BRANCHES; branchId++) {
//             for (int i = 0; i < 10000; i++) {
//                 String accountNumber = String.format("%d%09d", branchId, customerCounter.incrementAndGet());
//                 double initialBalance = 1000 + random.nextDouble() * 99000;
//                 Customer customer = new Customer(accountNumber, initialBalance);
//                 branches[branchId].addCustomer(customer);
//             }
//         }
//         System.out.println("Initialization complete. 10,000 customers created per branch.");
//     }

//     private void logTransaction(String message) {
//         String timestamp = dateFormat.format(new Date());
//         System.out.println("[" + timestamp + "] " + message);
//     }

//     public void checkBalance(String accountNumber) {
//         int branchId = Integer.parseInt(accountNumber.substring(0, 1));
//         Customer customer = branches[branchId].getCustomer(accountNumber);
//         if (customer != null) {
//             logTransaction(String.format("Balance check - Account: %s, Balance: %.2f", 
//                 accountNumber, customer.getBalance()));
//         }
//     }

//     public boolean deposit(String accountNumber, double amount) {
//         int branchId = Integer.parseInt(accountNumber.substring(0, 1));
//         Customer customer = branches[branchId].getCustomer(accountNumber);
//         if (customer != null) {
//             synchronized (customer) {
//                 customer.setBalance(customer.getBalance() + amount);
//                 logTransaction(String.format("Deposit - Account: %s, Amount: %.2f, New Balance: %.2f",
//                     accountNumber, amount, customer.getBalance()));
//                 return true;
//             }
//         }
//         return false;
//     }

//     public boolean withdraw(String accountNumber, double amount) {
//         int branchId = Integer.parseInt(accountNumber.substring(0, 1));
//         Customer customer = branches[branchId].getCustomer(accountNumber);
//         if (customer != null) {
//             synchronized (customer) {
//                 if (customer.getBalance() >= amount) {
//                     customer.setBalance(customer.getBalance() - amount);
//                     logTransaction(String.format("Withdrawal - Account: %s, Amount: %.2f, New Balance: %.2f",
//                         accountNumber, amount, customer.getBalance()));
//                     return true;
//                 }
//             }
//         }
//         return false;
//     }

//     public boolean transferMoney(String sourceAccount, String destAccount, double amount) {
//         int sourceBranchId = Integer.parseInt(sourceAccount.substring(0, 1));
//         int destBranchId = Integer.parseInt(destAccount.substring(0, 1));
        
//         Customer source = branches[sourceBranchId].getCustomer(sourceAccount);
//         Customer destination = branches[destBranchId].getCustomer(destAccount);
        
//         if (source != null && destination != null) {
//             Customer first = source.getAccountNumber().compareTo(destination.getAccountNumber()) < 0 ? 
//                 source : destination;
//             Customer second = first == source ? destination : source;
            
//             synchronized (first) {
//                 synchronized (second) {
//                     if (source.getBalance() >= amount) {
//                         source.setBalance(source.getBalance() - amount);
//                         destination.setBalance(destination.getBalance() + amount);
//                         logTransaction(String.format("Transfer - From: %s, To: %s, Amount: %.2f",
//                             sourceAccount, destAccount, amount));
//                         return true;
//                     }
//                 }
//             }
//         }
//         return false;
//     }

//     public boolean addCustomer(int branchId, double initialBalance) {
//         String accountNumber = String.format("%d%09d", branchId, customerCounter.incrementAndGet());
//         Customer customer = new Customer(accountNumber, initialBalance);
//         boolean success = branches[branchId].addCustomer(customer);
//         if (success) {
//             logTransaction(String.format("Added customer - Account: %s, Initial Balance: %.2f",
//                 accountNumber, initialBalance));
//         }
//         return success;
//     }

//     public boolean deleteCustomer(String accountNumber) {
//         int branchId = Integer.parseInt(accountNumber.substring(0, 1));
//         Customer removed = branches[branchId].removeCustomer(accountNumber);
//         if (removed != null) {
//             logTransaction(String.format("Deleted customer - Account: %s", accountNumber));
//             return true;
//         }
//         return false;
//     }

//     public boolean transferCustomer(String accountNumber, int newBranchId) {
//         int currentBranchId = Integer.parseInt(accountNumber.substring(0, 1));
//         Customer customer = branches[currentBranchId].removeCustomer(accountNumber);
        
//         if (customer != null) {
//             String newAccountNumber = String.format("%d%09d", newBranchId, customerCounter.incrementAndGet());
//             Customer newCustomer = new Customer(newAccountNumber, customer.getBalance());
//             boolean success = branches[newBranchId].addCustomer(newCustomer);
//             if (success) {
//                 logTransaction(String.format("Transferred customer - Old Account: %s, New Account: %s",
//                     accountNumber, newAccountNumber));
//                 return true;
//             }
//         }
//         return false;
//     }

//     public Branch[] getBranches() {
//         return branches;
//     }
// }

// class Updater implements Runnable {
//     private final Bank bank;
//     private final int branchId;
//     private final Random random;
//     private final int numTransactions;
//     private static final double BALANCE_CHECK_PROB = 0.3;
//     private static final double DEPOSIT_PROB = 0.23;
//     private static final double WITHDRAW_PROB = 0.23;
//     private static final double TRANSFER_PROB = 0.23;
//     private static final double ADD_CUSTOMER_PROB = 0.003;
//     private static final double DELETE_CUSTOMER_PROB = 0.003;
//     private static final double TRANSFER_CUSTOMER_PROB = 0.004;

//     public Updater(Bank bank, int branchId, int numTransactions) {
//         this.bank = bank;
//         this.branchId = branchId;
//         this.random = new Random();
//         this.numTransactions = numTransactions;
//     }

//     private String getRandomAccountNumber() {
//         Branch branch = bank.getBranches()[random.nextInt(10)];
//         List<Customer> customers = new ArrayList<>(branch.getAllCustomers());
//         if (!customers.isEmpty()) {
//             return customers.get(random.nextInt(customers.size())).getAccountNumber();
//         }
//         return null;
//     }

//     @Override
//     public void run() {
//         for (int i = 0; i < numTransactions; i++) {
//             double operation = random.nextDouble();
//             double amount = 100 + random.nextDouble() * 1000;

//             if (operation < BALANCE_CHECK_PROB) {
//                 String accountNumber = getRandomAccountNumber();
//                 if (accountNumber != null) {
//                     bank.checkBalance(accountNumber);
//                 }
//             } else if (operation < BALANCE_CHECK_PROB + DEPOSIT_PROB) {
//                 String accountNumber = getRandomAccountNumber();
//                 if (accountNumber != null) {
//                     bank.deposit(accountNumber, amount);
//                 }
//             } else if (operation < BALANCE_CHECK_PROB + DEPOSIT_PROB + WITHDRAW_PROB) {
//                 String accountNumber = getRandomAccountNumber();
//                 if (accountNumber != null) {
//                     bank.withdraw(accountNumber, amount);
//                 }
//             } else if (operation < BALANCE_CHECK_PROB + DEPOSIT_PROB + WITHDRAW_PROB + TRANSFER_PROB) {
//                 String sourceAccount = getRandomAccountNumber();
//                 String destAccount = getRandomAccountNumber();
//                 if (sourceAccount != null && destAccount != null && !sourceAccount.equals(destAccount)) {
//                     bank.transferMoney(sourceAccount, destAccount, amount);
//                 }
//             } else if (operation < BALANCE_CHECK_PROB + DEPOSIT_PROB + WITHDRAW_PROB + TRANSFER_PROB + ADD_CUSTOMER_PROB) {
//                 bank.addCustomer(branchId, amount);
//             } else if (operation < BALANCE_CHECK_PROB + DEPOSIT_PROB + WITHDRAW_PROB + TRANSFER_PROB + ADD_CUSTOMER_PROB + DELETE_CUSTOMER_PROB) {
//                 String accountNumber = getRandomAccountNumber();
//                 if (accountNumber != null) {
//                     bank.deleteCustomer(accountNumber);
//                 }
//             } else {
//                 String accountNumber = getRandomAccountNumber();
//                 if (accountNumber != null) {
//                     int newBranchId = random.nextInt(10);
//                     if (newBranchId != branchId) {
//                         bank.transferCustomer(accountNumber, newBranchId);
//                     }
//                 }
//             }
//         }
//     }
// }

// public class GNBBankingSimulation {
//     private static final int NUM_BRANCHES = 10;
//     private static final int UPDATERS_PER_BRANCH = 10;
//     private static final int TRANSACTIONS_PER_UPDATER = 1_000_000;

//     public static void main(String[] args) {
//         System.out.println("Starting Guwahati National Bank (GNB) Simulation...");
//         System.out.println("Configuration:");
//         System.out.println("- Number of branches: " + NUM_BRANCHES);
//         System.out.println("- Updaters per branch: " + UPDATERS_PER_BRANCH);
//         System.out.println("- Transactions per updater: " + TRANSACTIONS_PER_UPDATER);
//         System.out.println("----------------------------------------");

//         Bank bank = new Bank();
//         ExecutorService executor = Executors.newFixedThreadPool(NUM_BRANCHES * UPDATERS_PER_BRANCH);
//         long startTime = System.currentTimeMillis();

//         try {
//             List<Future<?>> futures = new ArrayList<>();
//             for (int branchId = 0; branchId < NUM_BRANCHES; branchId++) {
//                 for (int updaterId = 0; updaterId < UPDATERS_PER_BRANCH; updaterId++) {
//                     futures.add(executor.submit(new Updater(bank, branchId, TRANSACTIONS_PER_UPDATER)));
//                 }
//             }

//             for (Future<?> future : futures) {
//                 future.get();
//             }

//             long endTime = System.currentTimeMillis();
//             double executionTime = (endTime - startTime) / 1000.0;
//             System.out.println("----------------------------------------");
//             System.out.println(String.format("Simulation completed in %.2f seconds", executionTime));
//             System.out.println("Total transactions processed: " + 
//                 (long)NUM_BRANCHES * UPDATERS_PER_BRANCH * TRANSACTIONS_PER_UPDATER);

//         } catch (Exception e) {
//             e.printStackTrace();
//         } finally {
//             executor.shutdown();
//             try {
//                 if (!executor.awaitTermination(60, TimeUnit.SECONDS)) {
//                     executor.shutdownNow();
//                 }
//             } catch (InterruptedException e) {
//                 executor.shutdownNow();
//             }
//         }
//     }
// }

// import java.io.FileWriter;
// import java.io.IOException;
// import java.util.*;
// import java.util.concurrent.*;
// import java.util.concurrent.locks.ReentrantLock;

// class Customer {
//     String accountNumber;
//     double balance;

//     Customer(String accountNumber, double balance) {
//         this.accountNumber = accountNumber;
//         this.balance = balance;
//     }
// }

// class Branch {
//     String branchId;
//     ConcurrentHashMap<String, Customer> customers;
//     ReentrantLock lock;

//     Branch(String branchId) {
//         this.branchId = branchId;
//         this.customers = new ConcurrentHashMap<>();
//         this.lock = new ReentrantLock();
//     }

//     void addCustomer(Customer customer) {
//         customers.put(customer.accountNumber, customer);
//     }

//     void removeCustomer(String accountNumber) {
//         customers.remove(accountNumber);
//     }

//     Customer getCustomer(String accountNumber) {
//         return customers.get(accountNumber);
//     }

//     List<String> getAllAccountNumbers() {
//         return new ArrayList<>(customers.keySet());
//     }
// }

// class GNB {
//     ConcurrentHashMap<String, Branch> branches;

//     GNB() {
//         branches = new ConcurrentHashMap<>();
//         for (int i = 0; i < 10; i++) {
//             String branchId = "Branch" + i;
//             branches.put(branchId, new Branch(branchId));
//         }
//     }

//     Branch getBranch(String accountNumber) {
//         String branchId = "Branch" + accountNumber.charAt(0);
//         return branches.get(branchId);
//     }

//     void transferCustomer(String accountNumber, String newBranchId) {
//         Branch sourceBranch = getBranch(accountNumber);
//         Branch destinationBranch = branches.get(newBranchId);
//         Customer customer = sourceBranch.getCustomer(accountNumber);
//         if (customer != null) {
//             sourceBranch.removeCustomer(accountNumber);
//             destinationBranch.addCustomer(customer);
//         }
//     }
// }

// class Updater implements Runnable {
//     private static final String[] REQUEST_TYPES = {
//             "BalanceCheck", "CashDeposit", "CashWithdrawal", "MoneyTransfer", "AddCustomer", "DeleteCustomer", "TransferCustomer"
//     };
//     private static final double[] PROBABILITIES = {0.3, 0.23, 0.23, 0.23, 0.003, 0.003, 0.004};

//     private GNB gnb;
//     private String branchId;
//     private Random random;
//     private FileWriter logWriter;

//     Updater(GNB gnb, String branchId, FileWriter logWriter) {
//         this.gnb = gnb;
//         this.branchId = branchId;
//         this.random = new Random();
//         this.logWriter = logWriter;
//     }

//     private String getRandomAccountNumberFromBranch(Branch branch) {
//         List<String> accountNumbers = branch.getAllAccountNumbers();
//         if (accountNumbers.isEmpty()) {
//             return null;
//         }
//         return accountNumbers.get(random.nextInt(accountNumbers.size()));
//     }

//     private void logTransaction(String message) {
//         try {
//             logWriter.write(message + "\n");
//         } catch (IOException e) {
//             e.printStackTrace();
//         }
//     }

//     @Override
//     public void run() {
//         for (int i = 0; i < 1000000; i++) {
//             double rand = random.nextDouble();
//             double cumulativeProbability = 0.0;
//             String requestType = "";

//             for (int j = 0; j < PROBABILITIES.length; j++) {
//                 cumulativeProbability += PROBABILITIES[j];
//                 if (rand < cumulativeProbability) {
//                     requestType = REQUEST_TYPES[j];
//                     break;
//                 }
//             }

//             Branch branch = gnb.branches.get(branchId);
//             switch (requestType) {
//                 case "BalanceCheck":
//                     String accountNumber = getRandomAccountNumberFromBranch(branch);
//                     if (accountNumber != null) {
//                         Customer customer = branch.getCustomer(accountNumber);
//                         logTransaction("BalanceCheck: Account " + accountNumber + " has balance " + customer.balance);
//                     }
//                     break;

//                 case "CashDeposit":
//                     accountNumber = getRandomAccountNumberFromBranch(branch);
//                     if (accountNumber != null) {
//                         Customer customer = branch.getCustomer(accountNumber);
//                         double depositAmount = random.nextDouble() * 1000;
//                         customer.balance += depositAmount;
//                         logTransaction("CashDeposit: Deposited " + depositAmount + " to Account " + accountNumber);
//                     }
//                     break;

//                 case "CashWithdrawal":
//                     accountNumber = getRandomAccountNumberFromBranch(branch);
//                     if (accountNumber != null) {
//                         Customer customer = branch.getCustomer(accountNumber);
//                         if (customer.balance > 0) {
//                             double withdrawalAmount = random.nextDouble() * customer.balance;
//                             customer.balance -= withdrawalAmount;
//                             logTransaction("CashWithdrawal: Withdrawn " + withdrawalAmount + " from Account " + accountNumber);
//                         }
//                     }
//                     break;

//                 case "MoneyTransfer":
//                     String sourceAccount = getRandomAccountNumberFromBranch(branch);
//                     if (sourceAccount != null) {
//                         Customer sourceCustomer = branch.getCustomer(sourceAccount);
//                         if (sourceCustomer.balance > 0) {
//                             String destinationAccount = getRandomAccountNumberFromBranch(gnb.branches.get("Branch" + random.nextInt(10)));
//                             if (destinationAccount != null) {
//                                 Customer destinationCustomer = gnb.getBranch(destinationAccount).getCustomer(destinationAccount);
//                                 double transferAmount = random.nextDouble() * sourceCustomer.balance;
//                                 sourceCustomer.balance -= transferAmount;
//                                 destinationCustomer.balance += transferAmount;
//                                 logTransaction("MoneyTransfer: Transferred " + transferAmount + " from Account " + sourceAccount + " to Account " + destinationAccount);
//                             }
//                         }
//                     }
//                     break;

//                 case "AddCustomer":
//                     String newAccountNumber = branchId.charAt(6) + String.format("%09d", random.nextInt(1000000000));
//                     double initialBalance = random.nextDouble() * 10000;
//                     Customer newCustomer = new Customer(newAccountNumber, initialBalance);
//                     branch.addCustomer(newCustomer);
//                     logTransaction("AddCustomer: Added Account " + newAccountNumber + " with balance " + initialBalance);
//                     break;

//                 case "DeleteCustomer":
//                     accountNumber = getRandomAccountNumberFromBranch(branch);
//                     if (accountNumber != null) {
//                         branch.removeCustomer(accountNumber);
//                         logTransaction("DeleteCustomer: Deleted Account " + accountNumber);
//                     }
//                     break;

//                 case "TransferCustomer":
//                     accountNumber = getRandomAccountNumberFromBranch(branch);
//                     if (accountNumber != null) {
//                         String newBranchId = "Branch" + random.nextInt(10);
//                         gnb.transferCustomer(accountNumber, newBranchId);
//                         logTransaction("TransferCustomer: Transferred Account " + accountNumber + " to " + newBranchId);
//                     }
//                     break;
//             }
//         }
//     }
// }

// public class GNBBankingSimulation {
//     public static void main(String[] args) {
//         GNB gnb = new GNB();
//         ExecutorService executor = Executors.newFixedThreadPool(100);
//         List<Future<?>> futures = new ArrayList<>();
//         long startTime = System.currentTimeMillis();

//         // Initialize 10^4 customers in each branch
//         Random random = new Random();
//         for (int i = 0; i < 10; i++) {
//             String branchId = "Branch" + i;
//             Branch branch = gnb.branches.get(branchId);
//             for (int j = 0; j < 10000; j++) {
//                 String accountNumber = i + String.format("%09d", random.nextInt(1000000000));
//                 double initialBalance = random.nextDouble() * 10000;
//                 branch.addCustomer(new Customer(accountNumber, initialBalance));
//             }
//         }

//         try (FileWriter logWriter = new FileWriter("gnb_transactions.log")) {
//             for (int i = 0; i < 10; i++) {
//                 String branchId = "Branch" + i;
//                 for (int j = 0; j < 10; j++) {
//                     Updater updater = new Updater(gnb, branchId, logWriter);
//                     futures.add(executor.submit(updater));
//                 }
//             }

//             for (Future<?> future : futures) {
//                 future.get();
//             }
//         } catch (IOException | InterruptedException | ExecutionException e) {
//             e.printStackTrace();
//         } finally {
//             executor.shutdown();
//             long endTime = System.currentTimeMillis();
//             System.out.println("Simulation completed in " + (endTime - startTime) + " ms");
//         }
//     }
// }

// import java.io.FileWriter;
// import java.io.IOException;
// import java.util.*;
// import java.util.concurrent.*;
// import java.util.concurrent.locks.ReentrantLock;

// class Customer {
//     String accountNumber;
//     double balance;

//     Customer(String accountNumber, double balance) {
//         this.accountNumber = accountNumber;
//         this.balance = balance;
//     }
// }

// class Branch {
//     String branchId;
//     ConcurrentHashMap<String, Customer> customers;
//     ReentrantLock lock;

//     Branch(String branchId) {
//         this.branchId = branchId;
//         this.customers = new ConcurrentHashMap<>();
//         this.lock = new ReentrantLock();
//     }

//     void addCustomer(Customer customer) {
//         customers.put(customer.accountNumber, customer);
//     }

//     void removeCustomer(String accountNumber) {
//         customers.remove(accountNumber);
//     }

//     Customer getCustomer(String accountNumber) {
//         return customers.get(accountNumber);
//     }

//     List<String> getAllAccountNumbers() {
//         return new ArrayList<>(customers.keySet());
//     }
// }

// class GNB {
//     ConcurrentHashMap<String, Branch> branches;

//     GNB() {
//         branches = new ConcurrentHashMap<>();
//         for (int i = 0; i < 10; i++) {
//             String branchId = "Branch" + i;
//             branches.put(branchId, new Branch(branchId));
//         }
//     }

//     Branch getBranch(String accountNumber) {
//         String branchId = "Branch" + accountNumber.charAt(0);
//         return branches.get(branchId);
//     }

//     void transferCustomer(String accountNumber, String newBranchId) {
//         Branch sourceBranch = getBranch(accountNumber);
//         Branch destinationBranch = branches.get(newBranchId);
//         if (sourceBranch != null && destinationBranch != null) {
//             Customer customer = sourceBranch.getCustomer(accountNumber);
//             if (customer != null) {
//                 sourceBranch.removeCustomer(accountNumber);
//                 destinationBranch.addCustomer(customer);
//             }
//         }
//     }
// }

// class Updater implements Runnable {
//     private static final String[] REQUEST_TYPES = {
//             "BalanceCheck", "CashDeposit", "CashWithdrawal", "MoneyTransfer", "AddCustomer", "DeleteCustomer", "TransferCustomer"
//     };
//     private static final double[] PROBABILITIES = {0.3, 0.23, 0.23, 0.23, 0.003, 0.003, 0.004};

//     private final GNB gnb;
//     private final String branchId;
//     private final Random random;
//     private final FileWriter logWriter;

//     Updater(GNB gnb, String branchId, FileWriter logWriter) {
//         this.gnb = gnb;
//         this.branchId = branchId;
//         this.random = new Random();
//         this.logWriter = logWriter;
//     }

//     private String getRandomAccountNumberFromBranch(Branch branch) {
//         List<String> accountNumbers = branch.getAllAccountNumbers();
//         if (accountNumbers.isEmpty()) return null;
//         return accountNumbers.get(random.nextInt(accountNumbers.size()));
//     }

//     private void logTransaction(String message) {
//         synchronized (logWriter) { // Shared lock for all threads
//             try {
//                 logWriter.write(message + "\n");
//             } catch (IOException e) {
//                 e.printStackTrace();
//             }
//         }
//     }

//     @Override
//     public void run() {
//         for (int i = 0; i < 1000000; i++) {
//             double rand = random.nextDouble();
//             double cumulativeProbability = 0.0;
//             String requestType = "";

//             for (int j = 0; j < PROBABILITIES.length; j++) {
//                 cumulativeProbability += PROBABILITIES[j];
//                 if (rand < cumulativeProbability) {
//                     requestType = REQUEST_TYPES[j];
//                     break;
//                 }
//             }

//             Branch branch = gnb.branches.get(branchId);
//             switch (requestType) {
//                 case "BalanceCheck":
//                     String accountNumber = getRandomAccountNumberFromBranch(branch);
//                     if (accountNumber != null) {
//                         Customer customer = branch.getCustomer(accountNumber);
//                         logTransaction("BalanceCheck: Account " + accountNumber + " has balance " + customer.balance);
//                     }
//                     break;

//                 case "CashDeposit":
//                     accountNumber = getRandomAccountNumberFromBranch(branch);
//                     if (accountNumber != null) {
//                         Customer customer = branch.getCustomer(accountNumber);
//                         double depositAmount = random.nextDouble() * 1000;
//                         customer.balance += depositAmount;
//                         logTransaction("CashDeposit: Deposited " + depositAmount + " to Account " + accountNumber);
//                     }
//                     break;

//                 case "CashWithdrawal":
//                     accountNumber = getRandomAccountNumberFromBranch(branch);
//                     if (accountNumber != null) {
//                         Customer customer = branch.getCustomer(accountNumber);
//                         if (customer.balance > 0) {
//                             double withdrawalAmount = random.nextDouble() * customer.balance;
//                             customer.balance -= withdrawalAmount;
//                             logTransaction("CashWithdrawal: Withdrawn " + withdrawalAmount + " from Account " + accountNumber);
//                         }
//                     }
//                     break;

//                 case "MoneyTransfer":
//                     String sourceAccount = getRandomAccountNumberFromBranch(branch);
//                     if (sourceAccount != null) {
//                         Customer sourceCustomer = branch.getCustomer(sourceAccount);
//                         if (sourceCustomer != null && sourceCustomer.balance > 0) {
//                             Branch destBranch = gnb.branches.get("Branch" + random.nextInt(10));
//                             String destAccount = getRandomAccountNumberFromBranch(destBranch);
//                             if (destAccount != null) {
//                                 Customer destCustomer = destBranch.getCustomer(destAccount);
//                                 if (destCustomer != null) {
//                                     double transferAmount = random.nextDouble() * sourceCustomer.balance;
//                                     sourceCustomer.balance -= transferAmount;
//                                     destCustomer.balance += transferAmount;
//                                     logTransaction("MoneyTransfer: Transferred " + transferAmount + " from " + sourceAccount + " to " + destAccount);
//                                 }
//                             }
//                         }
//                     }
//                     break;

//                 case "AddCustomer":
//                     String newAccountNumber = branchId.charAt(6) + String.format("%09d", random.nextInt(1000000000));
//                     double initialBalance = random.nextDouble() * 10000;
//                     Customer newCustomer = new Customer(newAccountNumber, initialBalance);
//                     branch.addCustomer(newCustomer);
//                     logTransaction("AddCustomer: Added Account " + newAccountNumber + " with balance " + initialBalance);
//                     break;

//                 case "DeleteCustomer":
//                     accountNumber = getRandomAccountNumberFromBranch(branch);
//                     if (accountNumber != null) {
//                         branch.removeCustomer(accountNumber);
//                         logTransaction("DeleteCustomer: Deleted Account " + accountNumber);
//                     }
//                     break;

//                 case "TransferCustomer":
//                     accountNumber = getRandomAccountNumberFromBranch(branch);
//                     if (accountNumber != null) {
//                         String newBranchId = "Branch" + random.nextInt(10);
//                         gnb.transferCustomer(accountNumber, newBranchId);
//                         logTransaction("TransferCustomer: Transferred Account " + accountNumber + " to " + newBranchId);
//                     }
//                     break;
//             }
//         }
//     }
// }

// public class GNBBankingSimulation {
//     public static void main(String[] args) {
//         GNB gnb = new GNB();
//         ExecutorService executor = Executors.newFixedThreadPool(100);
//         long startTime = System.currentTimeMillis();

//         // Initialize 10^4 customers per branch
//         Random random = new Random();
//         for (int i = 0; i < 10; i++) {
//             Branch branch = gnb.branches.get("Branch" + i);
//             for (int j = 0; j < 10000; j++) {
//                 String accountNumber = i + String.format("%09d", random.nextInt(1000000000));
//                 branch.addCustomer(new Customer(accountNumber, random.nextDouble() * 10000));
//             }
//         }

//         try (FileWriter logWriter = new FileWriter("gnb_transactions.log")) {
//             for (int i = 0; i < 10; i++) {
//                 String branchId = "Branch" + i;
//                 for (int j = 0; j < 10; j++) {
//                     executor.submit(new Updater(gnb, branchId, logWriter));
//                 }
//             }

//             executor.shutdown();
//             executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS); // Wait for all threads

//         } catch (IOException | InterruptedException e) {
//             e.printStackTrace();
//         } finally {
//             long endTime = System.currentTimeMillis();
//             System.out.println("Execution Time: " + (endTime - startTime) + " ms");
//         }
//     }
// }

// import java.io.FileWriter;
// import java.io.IOException;
// import java.util.*;
// import java.util.concurrent.*;
// import java.util.concurrent.locks.ReentrantLock;

// class Customer {
//     String accountNumber;
//     double balance;

//     Customer(String accountNumber, double balance) {
//         this.accountNumber = accountNumber;
//         this.balance = balance;
//     }
// }

// class Branch {
//     String branchId;
//     ConcurrentHashMap<String, Customer> customers;
//     ReentrantLock lock;

//     Branch(String branchId) {
//         this.branchId = branchId;
//         this.customers = new ConcurrentHashMap<>();
//         this.lock = new ReentrantLock();
//     }

//     void addCustomer(Customer customer) {
//         customers.put(customer.accountNumber, customer);
//     }

//     void removeCustomer(String accountNumber) {
//         customers.remove(accountNumber);
//     }

//     Customer getCustomer(String accountNumber) {
//         return customers.get(accountNumber);
//     }

//     List<String> getAllAccountNumbers() {
//         return new ArrayList<>(customers.keySet());
//     }
// }

// class GNB {
//     ConcurrentHashMap<String, Branch> branches;

//     GNB() {
//         branches = new ConcurrentHashMap<>();
//         for (int i = 0; i < 10; i++) {
//             String branchId = "Branch" + i;
//             branches.put(branchId, new Branch(branchId));
//         }
//     }

//     Branch getBranch(String accountNumber) {
//         String branchId = "Branch" + accountNumber.charAt(0);
//         return branches.get(branchId);
//     }

//     void transferCustomer(String accountNumber, String newBranchId) {
//         Branch sourceBranch = getBranch(accountNumber);
//         Branch destinationBranch = branches.get(newBranchId);
//         if (sourceBranch != null && destinationBranch != null) {
//             Customer customer = sourceBranch.getCustomer(accountNumber);
//             if (customer != null) {
//                 sourceBranch.removeCustomer(accountNumber);
//                 destinationBranch.addCustomer(customer);
//             }
//         }
//     }
// }

// class Updater implements Runnable {
//     private static final String[] REQUEST_TYPES = {
//             "BalanceCheck", "CashDeposit", "CashWithdrawal", "MoneyTransfer", "AddCustomer", "DeleteCustomer", "TransferCustomer"
//     };
//     private static final double[] PROBABILITIES = {0.3, 0.23, 0.23, 0.23, 0.003, 0.003, 0.004};

//     private final GNB gnb;
//     private final String branchId;
//     private final Random random;
//     private final FileWriter logWriter;
//     private final boolean enableLogging; // Flag to control logging

//     Updater(GNB gnb, String branchId, FileWriter logWriter, boolean enableLogging) {
//         this.gnb = gnb;
//         this.branchId = branchId;
//         this.random = new Random();
//         this.logWriter = logWriter;
//         this.enableLogging = enableLogging; // Initialize the flag
//     }

//     private String getRandomAccountNumberFromBranch(Branch branch) {
//         List<String> accountNumbers = branch.getAllAccountNumbers();
//         if (accountNumbers.isEmpty()) return null;
//         return accountNumbers.get(random.nextInt(accountNumbers.size()));
//     }

//     private void logTransaction(String message) {
//         if (enableLogging) { // Only log if enabled
//             synchronized (logWriter) {
//                 try {
//                     logWriter.write(message + "\n");
//                 } catch (IOException e) {
//                     e.printStackTrace();
//                 }
//             }
//         }
//     }

//     @Override
//     public void run() {
//         for (int i = 0; i < 10000; i++) {
//             double rand = random.nextDouble();
//             double cumulativeProbability = 0.0;
//             String requestType = "";

//             for (int j = 0; j < PROBABILITIES.length; j++) {
//                 cumulativeProbability += PROBABILITIES[j];
//                 if (rand < cumulativeProbability) {
//                     requestType = REQUEST_TYPES[j];
//                     break;
//                 }
//             }

//             Branch branch = gnb.branches.get(branchId);
//             switch (requestType) {
//                 case "BalanceCheck":
//                     String accountNumber = getRandomAccountNumberFromBranch(branch);
//                     if (accountNumber != null) {
//                         Customer customer = branch.getCustomer(accountNumber);
//                         logTransaction("BalanceCheck: Account " + accountNumber + " has balance " + customer.balance);
//                     }
//                     break;

//                 case "CashDeposit":
//                     accountNumber = getRandomAccountNumberFromBranch(branch);
//                     if (accountNumber != null) {
//                         Customer customer = branch.getCustomer(accountNumber);
//                         double depositAmount = random.nextDouble() * 1000;
//                         customer.balance += depositAmount;
//                         logTransaction("CashDeposit: Deposited " + depositAmount + " to Account " + accountNumber);
//                     }
//                     break;

//                 case "CashWithdrawal":
//                     accountNumber = getRandomAccountNumberFromBranch(branch);
//                     if (accountNumber != null) {
//                         Customer customer = branch.getCustomer(accountNumber);
//                         if (customer.balance > 0) {
//                             double withdrawalAmount = random.nextDouble() * customer.balance;
//                             customer.balance -= withdrawalAmount;
//                             logTransaction("CashWithdrawal: Withdrawn " + withdrawalAmount + " from Account " + accountNumber);
//                         }
//                     }
//                     break;

//                 case "MoneyTransfer":
//                     String sourceAccount = getRandomAccountNumberFromBranch(branch);
//                     if (sourceAccount != null) {
//                         Customer sourceCustomer = branch.getCustomer(sourceAccount);
//                         if (sourceCustomer != null && sourceCustomer.balance > 0) {
//                             Branch destBranch = gnb.branches.get("Branch" + random.nextInt(10));
//                             String destAccount = getRandomAccountNumberFromBranch(destBranch);
//                             if (destAccount != null) {
//                                 Customer destCustomer = destBranch.getCustomer(destAccount);
//                                 if (destCustomer != null) {
//                                     double transferAmount = random.nextDouble() * sourceCustomer.balance;
//                                     sourceCustomer.balance -= transferAmount;
//                                     destCustomer.balance += transferAmount;
//                                     logTransaction("MoneyTransfer: Transferred " + transferAmount + " from " + sourceAccount + " to " + destAccount);
//                                 }
//                             }
//                         }
//                     }
//                     break;

//                 case "AddCustomer":
//                     String newAccountNumber = branchId.charAt(6) + String.format("%09d", random.nextInt(1000000000));
//                     double initialBalance = random.nextDouble() * 10000;
//                     Customer newCustomer = new Customer(newAccountNumber, initialBalance);
//                     branch.addCustomer(newCustomer);
//                     logTransaction("AddCustomer: Added Account " + newAccountNumber + " with balance " + initialBalance);
//                     break;

//                 case "DeleteCustomer":
//                     accountNumber = getRandomAccountNumberFromBranch(branch);
//                     if (accountNumber != null) {
//                         branch.removeCustomer(accountNumber);
//                         logTransaction("DeleteCustomer: Deleted Account " + accountNumber);
//                     }
//                     break;

//                 case "TransferCustomer":
//                     accountNumber = getRandomAccountNumberFromBranch(branch);
//                     if (accountNumber != null) {
//                         String newBranchId = "Branch" + random.nextInt(10);
//                         gnb.transferCustomer(accountNumber, newBranchId);
//                         logTransaction("TransferCustomer: Transferred Account " + accountNumber + " to " + newBranchId);
//                     }
//                     break;
//             }
//         }
//     }
// }

// public class GNBBankingSimulation {
//     public static void main(String[] args) {
//         GNB gnb = new GNB();
//         ExecutorService executor = Executors.newFixedThreadPool(100);
//         long startTime = System.currentTimeMillis();

//         // Initialize 10^4 customers per branch
//         Random random = new Random();
//         for (int i = 0; i < 10; i++) {
//             Branch branch = gnb.branches.get("Branch" + i);
//             for (int j = 0; j < 10000; j++) {
//                 String accountNumber = i + String.format("%09d", random.nextInt(1000000000));
//                 branch.addCustomer(new Customer(accountNumber, random.nextDouble() * 10000));
//             }
//         }

//         // Enable or disable logging here
//         boolean enableLogging = false; // Set to `true` to enable logging

//         try (FileWriter logWriter = enableLogging ? new FileWriter("gnb_transactions.log") : null) {
//             for (int i = 0; i < 10; i++) {
//                 String branchId = "Branch" + i;
//                 for (int j = 0; j < 10; j++) {
//                     executor.submit(new Updater(gnb, branchId, logWriter, enableLogging));
//                 }
//             }

//             executor.shutdown();
//             executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS); // Wait for all threads

//         } catch (IOException | InterruptedException e) {
//             e.printStackTrace();
//         } finally {
//             long endTime = System.currentTimeMillis();
//             System.out.println("Execution Time: " + (endTime - startTime) + " ms");
//         }
//     }
// }

// import java.io.FileWriter;
// import java.io.IOException;
// import java.util.*;
// import java.util.concurrent.*;
// import java.util.concurrent.locks.ReentrantLock;

// class Customer {
//     String accountNumber;
//     double balance;

//     Customer(String accountNumber, double balance) {
//         this.accountNumber = accountNumber;
//         this.balance = balance;
//     }
// }

// class Branch {
//     String branchId;
//     ConcurrentHashMap<String, Customer> customers;
//     CopyOnWriteArrayList<String> accountNumbers; // Thread-safe list for O(1) random access
//     ReentrantLock lock;

//     Branch(String branchId) {
//         this.branchId = branchId;
//         this.customers = new ConcurrentHashMap<>();
//         this.accountNumbers = new CopyOnWriteArrayList<>();
//         this.lock = new ReentrantLock();
//     }

//     void addCustomer(Customer customer) {
//         customers.put(customer.accountNumber, customer);
//         accountNumbers.add(customer.accountNumber); // Add account number to the list
//     }

//     void removeCustomer(String accountNumber) {
//         customers.remove(accountNumber);
//         accountNumbers.remove(accountNumber); // Remove account number from the list
//     }

//     Customer getCustomer(String accountNumber) {
//         return customers.get(accountNumber);
//     }

//     String getRandomAccountNumber(Random random) {
//         if (accountNumbers.isEmpty()) return null;
//         return accountNumbers.get(random.nextInt(accountNumbers.size())); // O(1) random access
//     }
// }

// class GNB {
//     ConcurrentHashMap<String, Branch> branches;

//     GNB() {
//         branches = new ConcurrentHashMap<>();
//         for (int i = 0; i < 10; i++) {
//             String branchId = "Branch" + i;
//             branches.put(branchId, new Branch(branchId));
//         }
//     }

//     Branch getBranch(String accountNumber) {
//         String branchId = "Branch" + accountNumber.charAt(0);
//         return branches.get(branchId);
//     }

//     void transferCustomer(String accountNumber, String newBranchId) {
//         Branch sourceBranch = getBranch(accountNumber);
//         Branch destinationBranch = branches.get(newBranchId);
//         if (sourceBranch != null && destinationBranch != null) {
//             Customer customer = sourceBranch.getCustomer(accountNumber);
//             if (customer != null) {
//                 sourceBranch.removeCustomer(accountNumber);
//                 destinationBranch.addCustomer(customer);
//             }
//         }
//     }
// }

// class Updater implements Runnable {
//     private static final String[] REQUEST_TYPES = {
//             "BalanceCheck", "CashDeposit", "CashWithdrawal", "MoneyTransfer", "AddCustomer", "DeleteCustomer", "TransferCustomer"
//     };
//     private static final double[] PROBABILITIES = {0.3, 0.23, 0.23, 0.23, 0.003, 0.003, 0.004};

//     private final GNB gnb;
//     private final String branchId;
//     private final Random random;
//     private final FileWriter logWriter;
//     private final boolean enableLogging; // Flag to control logging

//     Updater(GNB gnb, String branchId, FileWriter logWriter, boolean enableLogging) {
//         this.gnb = gnb;
//         this.branchId = branchId;
//         this.random = new Random();
//         this.logWriter = logWriter;
//         this.enableLogging = enableLogging; // Initialize the flag
//     }

//     private void logTransaction(String message) {
//         if (enableLogging) { // Only log if enabled
//             synchronized (logWriter) {
//                 try {
//                     logWriter.write(message + "\n");
//                 } catch (IOException e) {
//                     e.printStackTrace();
//                 }
//             }
//         }
//     }

//     @Override
//     public void run() {
//         for (int i = 0; i < 10000; i++) {
//             double rand = random.nextDouble();
//             double cumulativeProbability = 0.0;
//             String requestType = "";

//             for (int j = 0; j < PROBABILITIES.length; j++) {
//                 cumulativeProbability += PROBABILITIES[j];
//                 if (rand < cumulativeProbability) {
//                     requestType = REQUEST_TYPES[j];
//                     break;
//                 }
//             }

//             Branch branch = gnb.branches.get(branchId);
//             switch (requestType) {
//                 case "BalanceCheck":
//                     String accountNumber = branch.getRandomAccountNumber(random);
//                     if (accountNumber != null) {
//                         Customer customer = branch.getCustomer(accountNumber);
//                         logTransaction("BalanceCheck: Account " + accountNumber + " has balance " + customer.balance);
//                     }
//                     break;

//                 case "CashDeposit":
//                     accountNumber = branch.getRandomAccountNumber(random);
//                     if (accountNumber != null) {
//                         Customer customer = branch.getCustomer(accountNumber);
//                         double depositAmount = random.nextDouble() * 1000;
//                         customer.balance += depositAmount;
//                         logTransaction("CashDeposit: Deposited " + depositAmount + " to Account " + accountNumber + ". New Balance: " + customer.balance);
//                     }
//                     break;

//                 case "CashWithdrawal":
//                     accountNumber = branch.getRandomAccountNumber(random);
//                     if (accountNumber != null) {
//                         Customer customer = branch.getCustomer(accountNumber);
//                         if (customer.balance > 0) {
//                             double withdrawalAmount = random.nextDouble() * customer.balance;
//                             customer.balance -= withdrawalAmount;
//                             logTransaction("CashWithdrawal: Withdrawn " + withdrawalAmount + " from Account " + accountNumber + ". New Balance: " + customer.balance);
//                         }
//                     }
//                     break;

//                 case "MoneyTransfer":
//                     String sourceAccount = branch.getRandomAccountNumber(random);
//                     if (sourceAccount != null) {
//                         Customer sourceCustomer = branch.getCustomer(sourceAccount);
//                         if (sourceCustomer != null && sourceCustomer.balance > 0) {
//                             Branch destBranch = gnb.branches.get("Branch" + random.nextInt(10));
//                             String destAccount = destBranch.getRandomAccountNumber(random);
//                             if (destAccount != null) {
//                                 Customer destCustomer = destBranch.getCustomer(destAccount);
//                                 if (destCustomer != null) {
//                                     double transferAmount = random.nextDouble() * sourceCustomer.balance;
//                                     sourceCustomer.balance -= transferAmount;
//                                     destCustomer.balance += transferAmount;
//                                     logTransaction("MoneyTransfer: Transferred " + transferAmount + " from " + sourceAccount + " to " + destAccount + ". Source New Balance: " + sourceCustomer.balance + ", Destination New Balance: " + destCustomer.balance);
//                                 }
//                             }
//                         }
//                     }
//                     break;

//                 case "AddCustomer":
//                     String newAccountNumber = branchId.charAt(6) + String.format("%09d", random.nextInt(1000000000));
//                     double initialBalance = random.nextDouble() * 10000;
//                     Customer newCustomer = new Customer(newAccountNumber, initialBalance);
//                     branch.addCustomer(newCustomer);
//                     logTransaction("AddCustomer: Added Account " + newAccountNumber + " with balance " + initialBalance);
//                     break;

//                 case "DeleteCustomer":
//                     accountNumber = branch.getRandomAccountNumber(random);
//                     if (accountNumber != null) {
//                         branch.removeCustomer(accountNumber);
//                         logTransaction("DeleteCustomer: Deleted Account " + accountNumber);
//                     }
//                     break;

//                 case "TransferCustomer":
//                     accountNumber = branch.getRandomAccountNumber(random);
//                     if (accountNumber != null) {
//                         String newBranchId = "Branch" + random.nextInt(10);
//                         gnb.transferCustomer(accountNumber, newBranchId);
//                         logTransaction("TransferCustomer: Transferred Account " + accountNumber + " to " + newBranchId);
//                     }
//                     break;
//             }
//         }
//     }
// }

// public class GNBBankingSimulation {
//     public static void main(String[] args) {
//         GNB gnb = new GNB();
//         ExecutorService executor = Executors.newFixedThreadPool(100);
//         long startTime = System.currentTimeMillis();

//         // Initialize 10^4 customers per branch
//         Random random = new Random();
//         for (int i = 0; i < 10; i++) {
//             Branch branch = gnb.branches.get("Branch" + i);
//             for (int j = 0; j < 10000; j++) {
//                 String accountNumber = i + String.format("%09d", random.nextInt(1000000000));
//                 branch.addCustomer(new Customer(accountNumber, random.nextDouble() * 10000));
//             }
//         }

//         // Enable or disable logging here
//         boolean enableLogging = true; // Set to `true` to enable logging

//         try (FileWriter logWriter = enableLogging ? new FileWriter("gnb_transactions.log") : null) {
//             for (int i = 0; i < 10; i++) {
//                 String branchId = "Branch" + i;
//                 for (int j = 0; j < 10; j++) {
//                     executor.submit(new Updater(gnb, branchId, logWriter, enableLogging));
//                 }
//             }

//             executor.shutdown();
//             executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS); // Wait for all threads

//         } catch (IOException | InterruptedException e) {
//             e.printStackTrace();
//         } finally {
//             long endTime = System.currentTimeMillis();
//             System.out.println("Execution Time: " + (endTime - startTime) + " ms");
//         }
//     }
// }

// import java.io.FileWriter;
// import java.io.IOException;
// import java.util.*;
// import java.util.concurrent.*;
// import java.util.concurrent.atomic.AtomicLong;
// import java.util.concurrent.locks.ReentrantLock;

// class Customer {
//     String accountNumber;
//     private double balance;
//     ReentrantLock lock = new ReentrantLock(); // Lock for customer-level operations

//     Customer(String accountNumber, double balance) {
//         this.accountNumber = accountNumber;
//         this.balance = balance;
//     }

//     void deposit(double amount) {
//         lock.lock();
//         try {
//             balance += amount;
//         } finally {
//             lock.unlock();
//         }
//     }

//     void withdraw(double amount) {
//         lock.lock();
//         try {
//             if (balance >= amount) {
//                 balance -= amount;
//             }
//         } finally {
//             lock.unlock();
//         }
//     }

//     double getBalance() {
//         lock.lock();
//         try {
//             return balance;
//         } finally {
//             lock.unlock();
//         }
//     }

//     void transfer(Customer recipient, double amount) {
//         // Ensure consistent locking order to prevent deadlocks
//         ReentrantLock firstLock = this.lock;
//         ReentrantLock secondLock = recipient.lock;

//         if (this.accountNumber.compareTo(recipient.accountNumber) < 0) {
//             firstLock.lock();
//             secondLock.lock();
//         } else {
//             secondLock.lock();
//             firstLock.lock();
//         }

//         try {
//             if (this.balance >= amount) {
//                 this.balance -= amount;
//                 recipient.balance += amount;
//             }
//         } finally {
//             firstLock.unlock();
//             secondLock.unlock();
//         }
//     }
// }

// class Branch {
//     String branchId;
//     ConcurrentHashMap<String, Customer> customers;
//     CopyOnWriteArrayList<String> accountNumbers; // Thread-safe list for O(1) random access
//     ReentrantLock branchLock = new ReentrantLock(); // Lock for branch-level operations

//     Branch(String branchId) {
//         this.branchId = branchId;
//         this.customers = new ConcurrentHashMap<>();
//         this.accountNumbers = new CopyOnWriteArrayList<>();
//     }

//     void addCustomer(Customer customer) {
//         branchLock.lock();
//         try {
//             customers.put(customer.accountNumber, customer);
//             accountNumbers.add(customer.accountNumber);
//         } finally {
//             branchLock.unlock();
//         }
//     }

//     void removeCustomer(String accountNumber) {
//         branchLock.lock();
//         try {
//             customers.remove(accountNumber);
//             accountNumbers.remove(accountNumber);
//         } finally {
//             branchLock.unlock();
//         }
//     }

//     Customer getCustomer(String accountNumber) {
//         return customers.get(accountNumber);
//     }

//     String getRandomAccountNumber(Random random) {
//         branchLock.lock();
//         try {
//             if (accountNumbers.isEmpty()) return null;
//             return accountNumbers.get(random.nextInt(accountNumbers.size()));
//         } finally {
//             branchLock.unlock();
//         }
//     }
// }

// class GNB {
//     ConcurrentHashMap<String, Branch> branches;
//     AtomicLong accountCounter = new AtomicLong(1); // Global counter for sequential account numbers
//     // ReentrantLock transferLock = new ReentrantLock(); // Lock for inter-branch transfers

//     GNB() {
//         branches = new ConcurrentHashMap<>();
//         for (int i = 0; i < 10; i++) {
//             String branchId = "Branch" + i;
//             branches.put(branchId, new Branch(branchId));
//         }
//     }

//     Branch getBranch(String accountNumber) {
//         String branchId = "Branch" + accountNumber.charAt(0);
//         return branches.get(branchId);
//     }

//     void transferCustomer(String accountNumber, String newBranchId) {
//         // transferLock.lock();
//         // try {
//             Branch sourceBranch = getBranch(accountNumber);
//             Branch destinationBranch = branches.get(newBranchId);
//             if (sourceBranch != null && destinationBranch != null) {
//                 Customer customer = sourceBranch.getCustomer(accountNumber);
//                 if (customer != null) {
//                     sourceBranch.removeCustomer(accountNumber);
//                     destinationBranch.addCustomer(customer);
//                 }
//             }
//         // } finally {
//         //     transferLock.unlock();
//         // }
//     }

//     String generateAccountNumber() {
//         return String.valueOf(accountCounter.getAndIncrement()); // Generate sequential account numbers
//     }
// }

// class Updater implements Runnable {
//     private static final String[] REQUEST_TYPES = {
//             "BalanceCheck", "CashDeposit", "CashWithdrawal", "MoneyTransfer", "AddCustomer", "DeleteCustomer", "TransferCustomer"
//     };
//     private static final double[] PROBABILITIES = {0.3, 0.23, 0.23, 0.23, 0.003, 0.003, 0.004};

//     private final GNB gnb;
//     private final String branchId;
//     private final Random random;
//     private final FileWriter logWriter;
//     private final boolean enableLogging; // Flag to control logging

//     Updater(GNB gnb, String branchId, FileWriter logWriter, boolean enableLogging) {
//         this.gnb = gnb;
//         this.branchId = branchId;
//         this.random = new Random();
//         this.logWriter = logWriter;
//         this.enableLogging = enableLogging;
//     }

//     private void logTransaction(String message) {
//         if (enableLogging) {
//             synchronized (logWriter) {
//                 try {
//                     logWriter.write(message + "\n");
//                 } catch (IOException e) {
//                     e.printStackTrace();
//                 }
//             }
//         }
//     }

//     @Override
//     public void run() {
//         for (int i = 0; i < 1000000; i++) {
//             double rand = random.nextDouble();
//             double cumulativeProbability = 0.0;
//             String requestType = "";

//             for (int j = 0; j < PROBABILITIES.length; j++) {
//                 cumulativeProbability += PROBABILITIES[j];
//                 if (rand < cumulativeProbability) {
//                     requestType = REQUEST_TYPES[j];
//                     break;
//                 }
//             }

//             Branch branch = gnb.branches.get(branchId);
//             switch (requestType) {
//                 case "BalanceCheck":
//                     String accountNumber = branch.getRandomAccountNumber(random);
//                     if (accountNumber != null) {
//                         Customer customer = branch.getCustomer(accountNumber);
//                         if (customer != null) {
//                             double balance = customer.getBalance();
//                             logTransaction("BalanceCheck: Account " + accountNumber + " has balance " + balance);
//                         }
//                     }
//                     break;

//                 case "CashDeposit":
//                     accountNumber = branch.getRandomAccountNumber(random);
//                     if (accountNumber != null) {
//                         Customer customer = branch.getCustomer(accountNumber);
//                         if (customer != null) {
//                             double depositAmount = random.nextDouble() * 1000;
//                             customer.deposit(depositAmount);
//                             logTransaction("CashDeposit: Deposited " + depositAmount + " to Account " + accountNumber + ". New Balance: " + customer.getBalance());
//                         }
//                     }
//                     break;

//                 case "CashWithdrawal":
//                     accountNumber = branch.getRandomAccountNumber(random);
//                     if (accountNumber != null) {
//                         Customer customer = branch.getCustomer(accountNumber);
//                         if (customer != null) {
//                             double withdrawalAmount = random.nextDouble() * customer.getBalance();
//                             customer.withdraw(withdrawalAmount);
//                             logTransaction("CashWithdrawal: Withdrawn " + withdrawalAmount + " from Account " + accountNumber + ". New Balance: " + customer.getBalance());
//                         }
//                     }
//                     break;

//                 case "MoneyTransfer":
//                     accountNumber = branch.getRandomAccountNumber(random);
//                     if (accountNumber != null) {
//                         Customer sourceCustomer = branch.getCustomer(accountNumber);
//                         if (sourceCustomer != null) {
//                             Branch destBranch = gnb.branches.get("Branch" + random.nextInt(10));
//                             String destAccount = destBranch.getRandomAccountNumber(random);
//                             if (destAccount != null) {
//                                 Customer destCustomer = destBranch.getCustomer(destAccount);
//                                 if (destCustomer != null) {
//                                     double transferAmount = random.nextDouble() * sourceCustomer.getBalance();
//                                     sourceCustomer.transfer(destCustomer, transferAmount);
//                                     logTransaction("MoneyTransfer: Transferred " + transferAmount + " from " + accountNumber + " to " + destAccount + ". Source New Balance: " + sourceCustomer.getBalance() + ", Destination New Balance: " + destCustomer.getBalance());
//                                 }
//                             }
//                         }
//                     }
//                     break;

//                 case "AddCustomer":
//                     String newAccountNumber = gnb.generateAccountNumber();
//                     double initialBalance = random.nextDouble() * 10000;
//                     Customer newCustomer = new Customer(newAccountNumber, initialBalance);
//                     branch.addCustomer(newCustomer);
//                     logTransaction("AddCustomer: Added Account " + newAccountNumber + " with balance " + initialBalance);
//                     break;

//                 case "DeleteCustomer":
//                     accountNumber = branch.getRandomAccountNumber(random);
//                     if (accountNumber != null) {
//                         branch.removeCustomer(accountNumber);
//                         logTransaction("DeleteCustomer: Deleted Account " + accountNumber);
//                     }
//                     break;

//                 case "TransferCustomer":
//                     accountNumber = branch.getRandomAccountNumber(random);
//                     if (accountNumber != null) {
//                         String newBranchId = "Branch" + random.nextInt(10);
//                         gnb.transferCustomer(accountNumber, newBranchId);
//                         logTransaction("TransferCustomer: Transferred Account " + accountNumber + " to " + newBranchId);
//                     }
//                     break;
//             }
//         }
//     }
// }

// public class GNBBankingSimulation {
//     public static void main(String[] args) {
//         GNB gnb = new GNB();
//         ExecutorService executor = Executors.newFixedThreadPool(100);
//         long startTime = System.currentTimeMillis();

//         // Initialize 10^4 customers per branch with sequential account numbers
//         for (int i = 0; i < 10; i++) {
//             Branch branch = gnb.branches.get("Branch" + i);
//             for (int j = 0; j < 10000; j++) {
//                 String accountNumber = gnb.generateAccountNumber();
//                 branch.addCustomer(new Customer(accountNumber, new Random().nextDouble() * 10000));
//             }
//         }

//         // Enable or disable logging here
//         boolean enableLogging = false; // Set to `true` to enable logging

//         try (FileWriter logWriter = enableLogging ? new FileWriter("gnb_transactions.log") : null) {
//             for (int i = 0; i < 10; i++) {
//                 String branchId = "Branch" + i;
//                 for (int j = 0; j < 10; j++) {
//                     executor.submit(new Updater(gnb, branchId, logWriter, enableLogging));
//                 }
//             }

//             executor.shutdown();
//             executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
//         } catch (IOException | InterruptedException e) {
//             e.printStackTrace();
//         } finally {
//             long endTime = System.currentTimeMillis();
//             System.out.println("Execution Time: " + (endTime - startTime) + " ms");
//         }
//     }
// }


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
    // ReentrantLock transferLock = new ReentrantLock(); // Lock for inter-branch transfers

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
        // transferLock.lock();
        // try {
            Branch sourceBranch = getBranch(accountNumber);
            Branch destinationBranch = branches.get(newBranchId);
            if (sourceBranch != null && destinationBranch != null) {
                Customer customer = sourceBranch.getCustomer(accountNumber);
                if (customer != null) {
                    sourceBranch.removeCustomer(accountNumber);
                    destinationBranch.addCustomer(customer);
                }
            }
        // } finally {
        //     transferLock.unlock();
        // }
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
                    // String newAccountNumber = gnb.generateAccountNumber();
                    String newAccountNumber = branchId.charAt(6) + String.format("%09d", random.nextInt(1000000000));
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
public class Code {
    public static void main(String[] args) {
        // Create bank instance
        GNB gnb = new GNB();
        // Create thread pool for concurrent transaction processing
        ExecutorService executor = Executors.newFixedThreadPool(100);
        // Record start time for performance measurement
        long startTime = System.currentTimeMillis();

        // Initialize 10^4 customers per branch with random initial balances
        Random random = new Random();
        for (int i = 0; i < 10; i++) {
            Branch branch = gnb.branches.get("Branch" + i);
            for (int j = 0; j < 10000; j++) {
                // String accountNumber = gnb.generateAccountNumber();
                String accountNumber = i + String.format("%09d", random.nextInt(1000000000));
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