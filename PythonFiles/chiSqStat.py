import scipy.stats as stats

def chiSqStat(datosObservados, datosEncontrados):
    chiCuadrada = stats.chisquare(datosObservados, datosEncontrados)
    return chiCuadrada