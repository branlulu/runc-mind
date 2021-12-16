import numpy as np
import random
import copy

def max_min_fairness(L, d):
    """
    Inputs: 
    L = capacity left to allocate
    d = [d_0, ..., d_n - 1] = demand vector

    Returns:
    a = [a_0, ..., a_n - 1] = allocation vector
    """
    n = len(d)
    a_sorted = [0] * n
    a_original_order = [0] * n

    indices = np.argsort(np.array(d))
    d = sorted(d)
    for i in range(n):
        if d[i] <= L / (n - i):
            a_sorted[i] = d[i]
        else:
            a_sorted[i] = L / (n - i)
        L -= a_sorted[i]
    for i in range(n):
        index_before_sort = indices[i]
        a_original_order[index_before_sort] = a_sorted[i]
    return a_original_order


def column(matrix, i):
    return [row[i] for row in matrix]

def matrix_sum(A):
    ret = 0
    for i in range(len(A)):
        for j in range(len(A[0])):
            ret += A[i][j]
    return ret

def our_alg(D, B, M, C, C_k, L_j, L_k):
    # inputs:
    # D
    # B = [0, 1, ..., k]
    # M = [0, 1, ..., j]
    # C = [0, 1, ..., i]
    # L_j = [L_0, L_1, ..., L_j]
    # L_k = [L_0, L_1, ..., L_k]
    # C_k = [[C_0, C_1] // each index is a list e.g. [[1, 2], [3,4]]
    n = len(D)
    m = len(D[0])
    compute_left = [0] * n
    A = [[0] * m for i in range(n)]
    L_j_copy = copy.deepcopy(L_j)
    L_k_copy = copy.deepcopy(L_k)

    fair_share = [0] * n

    for k, container_list in enumerate(C_k):
        D_i = [0] * len(container_list)
        for index, i in enumerate(container_list):
            D_i[index] = sum(D[i])
        compute_alloc = max_min_fairness(L_k[k], D_i)
        alloc_index = 0
        for i in container_list: # container i
            compute_left[i] = compute_alloc[alloc_index]
            fair_share[i] = compute_alloc[alloc_index]
            alloc_index += 1
    
    for i in C:
        sum_mem_share = 0
        for j in M:
            demand_vector_j = column(D, j)
            alloc_j = max_min_fairness(L_j[j], demand_vector_j)
            sum_mem_share += alloc_j[i]
        fair_share[i] = min(fair_share[i], sum_mem_share)

    while len(B) > 0 and len(M) > 0:
        M_prime = M.copy()
        while len(M_prime) > 0:
            # apply compute-link constraints
            d = [[0] * m for i in range(n)]
            for i in range(n):
                for j in range(m):
                    d[i][j] = min(D[i][j] - A[i][j], compute_left[i])
            # iterate all memory links with remaining capacity
            j_hat = None
            for j in M_prime:
                d_j = 0
                for i in C:
                    d_j += d[i][j]
                if d_j < L_j[j]:
                    j_hat = j
                    break
            if j_hat is None:
                max_alloc = -1
                j_max = -1
                for j in M_prime:
                    demand_vector_j = column(d, j)
                    max_j = max(max_min_fairness(L_j[j], demand_vector_j))
                    # print("max_j =", max_j)
                    if max_j > max_alloc:
                        max_alloc = max_j
                        j_max = j
                j_hat = j_max
            demand_vector_j = column(d, j_hat)
            alloc_j = max_min_fairness(L_j[j_hat], demand_vector_j)
            for i in C:
                a_ij = alloc_j[i]
                A[i][j_hat] += a_ij
                compute_left[i] -= a_ij
                L_j[j_hat] -= a_ij
            M_prime.remove(j_hat)
            if L_j[j_hat] == 0:
                M.remove(j_hat)
        for k in list(B):
            excess_share = 0
            for i in list(C_k[k]):
                if compute_left[i] > 0:
                    excess_share += compute_left[i]
                    C_k[k].remove(i)
                    C.remove(i)
            for i in C_k[k]:
                compute_left[i] += excess_share / len(C_k[k])
            if len(C_k[k]) == 0 or excess_share == 0:
                B.remove(k)
        # print("D = ", D)
        # print("A = ", A)
        total_demands = matrix_sum(D)
        total_allocated = matrix_sum(A)
        total_capacity = sum(L_j_copy) + sum(L_k_copy)
        satisfied_demand = total_allocated/ total_demands * 100
        satisfied_capacity = total_allocated / total_capacity* 100
        

        # print(total_allocated/ total_demands * 100, "% of demands are satisfied")
        # print(total_allocated / total_capacity* 100, "% of capacity are utilized")
        min_guarantee = True
        for i in range(n):
            if sum(A[i]) < fair_share[i] - 1:
                # print("D = ", D)
                # print("A = ", A)
                # print("Min-guarantee not satisfied for container ", i)
                # print("sum(A[i]) = ", sum(A[i]), "fair share = ", fair_share[i])
                min_guarantee = False
                break
        # if min_guarantee:
            # print("Min-guarantee satisfied.")

        leftover_demand = False
        for j, l_j in enumerate(L_j):
            if l_j > 1:
                for i in range(n):
                    if D[i][j] - A[i][j] > 0 and compute_left[i] > 0:
                        # print("There is leftover demand at underutilized link ", j, "l_j = ", l_j)
                        leftover_demand = True
        # if not leftover_demand:
        #     print("There is no leftover demand.")
        
        return satisfied_demand, satisfied_capacity, min_guarantee, leftover_demand
            

# our_alg(D, B, M, C, C_k, L_j, L_k)

def random_alg(D, B, M, C, C_k, L_j, L_k):
    n = len(D)
    m = len(D[0])
    A = [[0] * m for i in range(n)]
    L_j_copy = copy.deepcopy(L_j)
    L_k_copy = copy.deepcopy(L_k)
    compute_blade_map = {} # {container: compute-blade}
    for k, container_list in enumerate(C_k):
        for i in container_list:
            compute_blade_map[i] = k

    fair_share = [0] * n

    for k, container_list in enumerate(C_k):
        D_i = [0] * len(container_list)
        for index, i in enumerate(container_list):
            D_i[index] = sum(D[i])
        compute_alloc = max_min_fairness(L_k[k], D_i)
        alloc_index = 0
        for i in container_list: # container i
            fair_share[i] = compute_alloc[alloc_index]
            alloc_index += 1
    
    for i in C:
        sum_mem_share = 0
        for j in M:
            demand_vector_j = column(D, j)
            alloc_j = max_min_fairness(L_j[j], demand_vector_j)
            sum_mem_share += alloc_j[i]
        fair_share[i] = min(fair_share[i], sum_mem_share)
    
    while len(B) > 0 and len(M) > 0 and len(C) > 0:
        j = random.choice(M)
        containers_to_choose = [i for i in C if D[i][j]-A[i][j] > 0]
        if len(containers_to_choose) == 0: M.remove(j)
        else:
            i = random.choice(containers_to_choose)
            k = compute_blade_map[i]
            a_ij = min((D[i][j] - A[i][j])/2, L_k[k], L_j[j])
            A[i][j] += a_ij
            L_j[j] -= a_ij
            L_k[k] -= a_ij
            if L_j[j] == 0:
                M.remove(j)
            if L_k[k] == 0 and k in B:
                B.remove(k)
            if(sum(D[i]) == sum(A[i])):
                C.remove(i)
            for i in list(C):
                k = compute_blade_map[i]
                if L_k[k] == 0: 
                    C.remove(i)
                else:
                    satisfied = True
                    for j in M:
                        if (D[i][j] - A[i][j] > 0):
                            satisfied = False
                            break
                    if satisfied: 
                        C.remove(i)
    # print(A)
    total_demands = matrix_sum(D)
    total_allocated = matrix_sum(A)
    total_capacity = sum(L_j_copy) + sum(L_k_copy)
    # print("D = ", D)
    # print("A = ", A)
    
    min_guarantee = True
    for i in range(n):
        if sum(A[i]) < fair_share[i] - 1:
            # print("D = ", D)
            # print("A = ", A)
            # print("Min-guarantee not satisfied for container ", i)
            # print("sum(A[i]) = ", sum(A[i]), "fair share = ", fair_share[i])
            min_guarantee = False
            break
    leftover_demand = False
    for j, l_j in enumerate(L_j):
        if l_j > 1:
            for i in range(n):
                k = compute_blade_map[i]
                if D[i][j] - A[i][j] > 0 and L_k[k] > 1:
                    # print("There is leftover demand at underutilized link ", j, "l_j = ", l_j)
                    leftover_demand = True

    # print(total_allocated/total_demands * 100, "% of demands are satisfied")
    # print(total_allocated / total_capacity* 100, "% of capacity are utilized")
    satisfied_demand = total_allocated/ total_demands * 100
    satisfied_capacity = total_allocated / total_capacity* 100
    return satisfied_demand, satisfied_capacity, min_guarantee, leftover_demand



   

def matrix_sum_apply_min(A):
    total = 0
    for row in range(len(A)):
        total += min(sum(A[row]), 100)
    return total

def simulate(N):
    demands_our_alg = [0] * N
    demands_random_alg = [0] * N
    min_guarantee_our = [0] * N
    leftover_demand_our = [0] * N
    util_our_alg = [0] * N
    util_random_alg = [0] * N
    min_guarantee_random = [0] * N
    leftover_demand_random = [0] * N

    for i in range(N):
        n = 5 # no of containers 
        m = 5 # no of mem blades
        k = 5 # no of compute blades

        D = np.random.randint(50, size=(n, m))
        normalized_D = np.random.randint(50, size=(n, m))


        B = list(range(k))
        M = list(range(m))
        C = list(range(n))
        C_k = [[0], [1], [2], [3], [4]]
        L_j = [100] * m
        L_k = [100] * k
        # print("D= ", D)
        # print("B= ", B)
        # print("M= ", M)
        # print("C= ", C)
        # print("L_j= ", L_j)
        # print("L_k =", L_k)

        demands_our_alg[i], util_our_alg[i], min_guarantee_our[i], leftover_demand_our[i] = our_alg(copy.deepcopy(D), copy.deepcopy(B), copy.deepcopy(M), copy.deepcopy(C), copy.deepcopy(C_k), copy.deepcopy(L_j), copy.deepcopy(L_k))
        demands_random_alg[i], util_random_alg[i], min_guarantee_random[i], leftover_demand_random[i] = random_alg(copy.deepcopy(D), copy.deepcopy(B), copy.deepcopy(M), copy.deepcopy(C), copy.deepcopy(C_k), copy.deepcopy(L_j), copy.deepcopy(L_k))


    print("------Our algorithm-------")
    print("average demand satisfied: ", mean(demands_our_alg), "%")
    print("average link utilization: ", mean(util_our_alg), "%")
    print("Min guarantee: ", mean(min_guarantee_our))
    print("Leftover demand: ", mean(leftover_demand_our))
    print("------Random algorithm-------")
    print("average demand satisfied: ", mean(demands_random_alg), "%")
    print("average link utilization: ", mean(util_random_alg),"%")
    print("Min guarantee: ", mean(min_guarantee_random))
    print("Leftover demand: ", mean(leftover_demand_random))

def mean(l):
    return sum(l) / len(l)

simulate(1000)
