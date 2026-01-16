# Kakuro simplificado para o problema dado

# Variáveis: posições no formato (linha, coluna)
variables = [
    (1,1), (1,2), (1,3),
    (2,1), (2,2), (2,3), (2,4), (2,5),
    (3,2), (3,3), (3,4), (3,5),
    (4,1), (4,2), (4,3), (4,4),
    (5,1), (5,2), (5,3)
]

# Domínio: 1 a 9
domain = list(range(1, 10))

# Restrições de soma
sum_constraints = [
    ([(1,1), (1,2), (1,3)], 20),
    ([(2,1), (2,2), (2,3), (2,4), (2,5)], 23),
    ([(3,2), (3,3), (3,4), (3,5)], 14),
    ([(4,1), (4,2), (4,3), (4,4)], 23),
    ([(5,1), (5,2), (5,3)], 19),
    ([(1,1), (2,1)], 13),
    ([(4,1), (5,1)], 11),
    ([(1,2), (2,2), (3,2), (4,2), (5,2)], 26),
    ([(1,3), (2,3), (3,3), (4,3), (5,3)], 28),
    ([(2,4), (3,4), (4,4)], 18),
    ([(2,5), (3,5)], 3)
]

# Diferenças: pares de variáveis que não podem ter o mesmo valor
diff_pairs = [
    ((1,1),(1,2)), ((1,1),(1,3)), ((1,1),(2,1)),
    ((1,1),(4,1)), ((1,1),(5,1)), ((1,2),(1,3)),
    ((1,2),(2,2)), ((1,2),(3,2)), ((1,2),(4,2)),
    ((1,2),(5,2)), ((1,3),(2,3)), ((1,3),(3,3)),
    ((1,3),(4,3)), ((1,3),(5,3)), ((2,1),(2,2)),
    ((2,1),(2,3)), ((2,1),(2,4)), ((2,1),(2,5)),
    ((2,1),(4,1)), ((2,1),(5,1)), ((2,2),(2,3)), 
    ((2,2),(2,4)), ((2,2),(2,5)), ((2,2),(3,2)), 
    ((2,2),(4,2)), ((2,2),(5,2)), ((2,3),(2,4)),
    ((2,3),(2,5)), ((2,3),(3,3)), ((2,3),(4,3)),
    ((2,3),(5,3)), ((2,4),(2,5)), ((2,4),(3,4)),
    ((2,4),(4,4)), ((2,5),(3,5)), ((3,2),(3,3)),
    ((3,2),(3,4)), ((3,2),(3,5)), ((3,2),(4,2)),
    ((3,2),(5,2)), ((3,3),(3,4)), ((3,3),(3,5)),
    ((3,3),(4,3)), ((3,3),(5,3)), ((3,3),(3,4)), 
    ((3,4),(3,5)), ((3,4),(4,4)), ((4,1),(4,2)), 
    ((4,1),(4,3)), ((4,1),(4,4)), ((4,1),(5,1)), 
    ((4,2),(4,3)), ((4,2),(4,4)), ((4,2),(5,2)), 
    ((4,3),(4,4)), ((4,3),(5,3)), ((5,1),(5,2)), 
    ((5,1),(5,3))
]

def all_different(values):
    return len(values) == len(set(values))

def check_diff(assignment):
    # Check all pairs in diff_pairs have different values
    for (v1, v2) in diff_pairs:
        if v1 in assignment and v2 in assignment:
            if assignment[v1] == assignment[v2]:
                return False
    return True

def check_sums(assignment):
    for vars_, target_sum in sum_constraints:
        values = [assignment[v] for v in vars_ if v in assignment]
        if len(values) == len(vars_):
            # All variables assigned: sum must match and values distinct
            if sum(values) != target_sum or not all_different(values):
                return False
        else:
            # Partial assignment: sum so far <= target sum and values distinct so far
            if sum(values) > target_sum or not all_different(values):
                return False
    return True

def backtracking(assignment):
    # If all variables assigned, return assignment
    if len(assignment) == len(variables):
        return assignment
    
    # Select next unassigned variable
    unassigned = [v for v in variables if v not in assignment]
    var = unassigned[0]
    
    for value in domain:
        assignment[var] = value
        
        if check_diff(assignment) and check_sums(assignment):
            result = backtracking(assignment)
            if result is not None:
                return result
        
        # Backtrack
        del assignment[var]
    
    return None

if __name__ == "__main__":
    solution = backtracking({(1,3): 4, (2,4): 4, (4,2): 2, (4,3): 5, (5,2): 8})
    if solution:
        print("Solução encontrada:")
        for var in sorted(solution):
            print(f"x{var} = {solution[var]}")
    else:
        print("Sem solução.")
