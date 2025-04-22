# **CS331 Programming Languages Lab - Assignment VI**  
### **Maze Pathfinder with Faulty Nodes in Prolog**  

## **Description**  
This assignment involves implementing a Prolog program to find the **shortest path** in a maze (or grid) with **faulty nodes**, where movement is restricted to adjacent non-faulty nodes. The program dynamically manages faulty nodes and provides an **interactive menu** for testing.  

Key Features:  
✅ **BFS-based shortest pathfinding** (avoids cycles and faulty nodes)  
✅ **Dynamic fault management** (add/remove faulty nodes at runtime)  
✅ **Interactive menu system** for easy testing  
✅ **Sample test cases** for quick validation  
✅ **Starter guide** (`start/0` predicate) for beginners  

---

## **Prerequisites**  
1. **SWI-Prolog** (Install via [https://www.swi-prolog.org/](https://www.swi-prolog.org/))  
2. **C++ Compiler** (for maze generation)  
3. **Graphviz** (for visualization, optional)  
   ```bash
   sudo apt-get install graphviz  # Linux
   brew install graphviz          # macOS
   ```

---

## **How to Run the Code**  

### **1. Generate the Maze**  
First, compile and run the provided C++ maze generator:  
```bash
g++ generateMaze.cpp -o generateMaze
./generateMaze 10 10 0.2 1 1 8 8
```
This creates:  
- **`Mazedata.pl`** (Prolog facts for the maze)  
- **`graph1.png`** (visual representation of the maze)  

### **2. Load the Prolog Program**  
Start SWI-Prolog and load the program:  
```prolog
?- [shortest_path].  % Load the program
```

### **3. Program Entry Points**  

#### **A. Starter Guide (`start/0`)**
```prolog
?- start.
```
*Displays:*
```
Maze Pathfinder Program
-----------------------
1. First load "Mazedata.pl" containing your maze
2. Use menu/0 for interactive pathfinding
3. Use sample_queries/0 for automated testing
```

#### **B. Interactive Menu (`menu/0`)**
```prolog
?- menu.  % Main control panel
```
**Menu Options:**  
1. **Add faulty node** (Mark a node as faulty)  
2. **Remove faulty node** (Unmark a faulty node)  
3. **Find shortest path** (Between two nodes)  
4. **Display faulty nodes** (List all faulty nodes)  
5. **Exit** (Quit the program)  

#### **C. Test Suite (`sample_queries/0`)**
```prolog
?- sample_queries.  % Run built-in test cases
```

---

## **Code Structure**  
| File | Purpose |
|------|---------|
| **`generateMaze.cpp`** | Generates maze data (`Mazedata.pl`) and visualization (`graph1.png`) |
| **`shortest_path.pl`** | Contains the Prolog implementation (BFS pathfinding + menu system) |
| **`Mazedata.pl`** | Contains maze facts (`mazelink/2`, `faultynode/1`) |
| **`graph1.png`** | Visual representation of the maze (optional) |

---

## **Example Queries**  
### **1. Direct Pathfinding**
```prolog
?- shortest_path(0, 8, Path).
% Output: Path = [0, 1, 2, 5, 8]
```

### **2. Dynamic Fault Management**
```prolog
?- assertz(faultynode(4)).  % Block node 4
?- retract(faultynode(4)).  % Unblock node 4
```

### **3. System Inspection**
```prolog
?- findall(Node, faultynode(Node), FaultyNodes).
% Output: FaultyNodes = [3, 7] (example)
```

---

## **Expected Output**  
### **Sample Workflow**
1. Initialize:
   ```prolog
   ?- start.
   ```
2. Run menu:
   ```prolog
   ?- menu.
   ```
3. Menu interaction:
   ```
   === Maze Pathfinder Menu ===
   Enter choice (1-5): 3
   Source: 0. Destination: 8.
   Shortest path: [0,1,2,5,8]
   ```

---

## **Troubleshooting**  
- **"Unknown procedure" error?** → Check predicate parentheses  
- **No path exists?** → Verify node connectivity with `mazelink/2`  
- **Visualization issues?** → Install Graphviz or ignore `graph1.png`  

---

## **Conclusion**  
This implementation provides:  
- Complete BFS pathfinding with fault tolerance  
- Three access points for different user levels  
- Clear documentation through `start/0`  
- Academic integrity (original code)  

**Demo-ready submission** for CS331 assignment.  

---  
**Submitted by:** Divyansh Chandak  