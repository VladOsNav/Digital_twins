import matplotlib.pyplot as plt
import numpy as np

def modBicomp(pesoKg, dosis, clearance, volInicialLKg, vidaMediaA, vidaMediaB, tMin):
    tiempo = np.array(tMin)
    alpha = np.log(2)/vidaMediaA
    beta = np.log(2)/vidaMediaB

    k10 = clearance/volInicialLKg
    k21 = (alpha*beta) / k10
    k12 = alpha + beta - k10 - k21
    volTotML = (volInicialLKg*1000) * pesoKg

    C1 = ((dosis/volTotML) * (alpha-k21)) / (alpha - beta)
    C2 = ((dosis/volTotML) * (k21-beta)) / (alpha - beta)

    Cp = (C1 * np.exp(-alpha * tiempo)) + (C2 * np.exp(-beta * tiempo))
    return Cp
