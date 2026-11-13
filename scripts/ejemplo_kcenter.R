library(magrittr)

calcular_distancia <- function(X, Centros, metrica = "euclidean", p = 2) {
  # X: Matriz de datos (n x d)
  # Centros: Matriz de centros actuales (k x d)
  # metrica: Opciones de dist(): "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"
  # p: Parámetro para la distancia Minkowski (por defecto p = 2, que es la euclidiana)
  n <- nrow(X)
  k <- nrow(Centros)
  # Truco: Unir los datos y los centros en una sola matriz
  M <- rbind(X, Centros)
  # Calcular la matriz de distancias completa usando dist()
  if (metrica == "minkowski") {
    D_full <- as.matrix(dist(M, method = metrica, p = p))
  } else {
    D_full <- as.matrix(dist(M, method = metrica))
  }
  # Extraer la submatriz cruzada: Filas de X (1 a n) frente a Columnas de Centros (n+1 a n+k)
  D <- D_full[1:n, (n + 1):(n + k), drop = FALSE]
  return(D)
}

# 2. Actualización de Centros (Soportando tipo de centro)
actualizar_centros <- function(X, clusters, k, tipo_centro = "media", metrica = "euclidean") {
  d <- ncol(X)
  nuevos_centros <- matrix(0, nrow = k, ncol = d)
  for (j in 1:k) {
    puntos_cluster <- X[clusters == j, , drop = FALSE]
    if (nrow(puntos_cluster) == 0) {
      nuevos_centros[j, ] <- X[sample(1:nrow(X), 1), ]
    } else {
      if (tipo_centro == "media") {
        nuevos_centros[j, ] <- colMeans(puntos_cluster)
      } else if (tipo_centro == "mediana") {
        nuevos_centros[j, ] <- apply(puntos_cluster, 2, median)
      } else if (tipo_centro == "medoide") {
        # El medoide minimiza la distancia total interna usando la misma métrica elegida
        dists_internas <- as.matrix(dist(puntos_cluster, method = metrica))
        suma_dists <- rowSums(dists_internas)
        idx_medoide <- which.min(suma_dists)
        nuevos_centros[j, ] <- puntos_cluster[idx_medoide, ]
      }
    }
  }
  return(nuevos_centros)
}

# 3. Pipeline Principal del Algoritmo de Lloyd
lloyd_clustering <- function(datos, k, metrica = "euclidean", tipo_centro = "media", p = 2, max_iter = 100, tol = 0.0000001) {
  X <- as.matrix(datos)
  n <- nrow(X)
  set.seed(42)
  centros_actuales <- X[sample(1:n, k), , drop = FALSE]
  historial_inercia <- numeric(max_iter)
  for (iter in 1:max_iter) {
    # Asignación usando la función dist() ampliada
    D <- calcular_distancia(X, centros_actuales, metrica = metrica, p = p)
    clusters <- max.col(-D) # Mínima distancia
    # Inercia (suma de distancias mínimas)
    inercia <- sum(apply(D, 1, min))
    historial_inercia[iter] <- inercia
    # Actualización
    nuevos_centros <- actualizar_centros(X, clusters, k, tipo_centro = tipo_centro, metrica = metrica)
    # Convergencia
    if (max(abs(nuevos_centros - centros_actuales)) < tol) {
      cat(sprintf("Convergencia alcanzada en la iteración %d usando métrica '%s'.\n", iter, metrica))
      break
    }
    centros_actuales <- nuevos_centros
  }
  list(
    clusters = clusters,
    centros = centros_actuales,
    iteraciones = iter,
    inercia_final = tail(historial_inercia[historial_inercia > 0], 1)
  )
}

# Preparar datos
data(iris)
iris_X <- scale(iris[, 1:4])

# Prueba 1: Distancia "maximum" (Chebyshev) con centroides en la mediana
res_chebyshev <- lloyd_clustering(
  datos = iris_X, 
  k = 3, 
  metrica = "maximum", 
  tipo_centro = "mediana"
)
# Prueba 2: Distancia "minkowski" con p = 3 y tipo de centro "medoide" (K-Medoids)
res_minkowski <- lloyd_clustering(
  datos = iris_X, 
  k = 3, 
  metrica = "minkowski", 
  p = 3, 
  tipo_centro = "medoide"
)
# Comparar con las clases reales de iris
table(Especie = iris$Species, ClusterChebyshev = res_chebyshev$clusters)
table(Especie = iris$Species, ClusterMinkowski = res_minkowski$clusters)

mod1 <- lloyd_clustering(
  datos = USArrests, 
  k = 10, 
  metrica = "manhattan", 
  tipo_centro = "medoide"
)