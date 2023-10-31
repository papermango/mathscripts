import math
import random
import timeit
# import pandas as pd
# import matplotlib.pyplot as plt

"""Brute force algorithm to test if n is prime deterministically."""
def brute_force(n):
    if n == 2:
        return True
    for i in range(2, math.isqrt(n)):
        if n % i == 0:
            return False
    return True

"""Probabilistically checks if n is prime by checking if it is a Fermat prime for k random bases."""
def fermat_primality(n, k):
    if n == 2 or n == 3:
        return True
    witnesses = [random.randint(2, n - 1) for i in range(k)]
    for w in witnesses:
        if pow(w, n - 1, n) != 1:
            return False
    return True

def test_correctness(n, k):
    negatives = 0
    for i in range(2, n):
        if brute_force(n) != fermat_primality(n, k):
            negatives += 1
    return negatives

"""Generates a table of binned random numbers with [samples] numbers in each bin."""
def generate_test_data(bins, samples, base=2):
    random_numbers = []
    for i in range(len(bins) - 1):
        low = base**bins[i]
        high = base**bins[i + 1]
        random_numbers.append([random.randint(low, high) for i in range(samples)])
    return random_numbers

"""Takes in generated test data, and computes timing data."""
def collect_timing_data(data, ks):
    out = []
    for row in data:
        bf_time = timeit.timeit(lambda: [brute_force(n) for n in row], number=1)
        fp_times = [timeit.timeit(
            lambda: [fermat_primality(n, k) for n in row], number=1) for k in ks]
        out.append([bf_time] + fp_times)
    return out

# Currently much slower than timings, because brute_force is continuously recomputed
"""Takes in generated test data, and computes correctness data."""
def collect_correctness_data(data, ks):
    out = []
    for row in data:
        krow = []
        for k in ks:
            count = 0
            for elem in row:
                if brute_force(elem) != fermat_primality(elem, k):
                    count += 1
            krow.append(count)
        out.append(krow)
    return out