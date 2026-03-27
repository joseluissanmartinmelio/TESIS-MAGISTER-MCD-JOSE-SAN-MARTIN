# Repositorio replicación de datos y scraper Tesis *"El efecto de la transparencia activa (TA) sobre las solicitudes de acceso a la información (SAI) en los municipios de Chile entre 2013-2024"*

## Script tesis magíster

El script Principal.R y Simulacion.R permiten replicar mediante los datos de "data/panel_real_final.xlsx" los resultados de los modelos usados en la tesis de magister en Ciencia de Datos Aplicados.

## Scraper SINIM - Guía de Uso

### ¿Qué hace este scraper?

Extrae datos municipales del portal **SINIM** (Sistema Nacional de Información Municipal) de Chile. Permite descargar indicadores y variables de todos los municipios del país para uno o varios años, además es descragna en formato panel de datos, lo que facilita y disminuye el tiempo de limpiza de datos que ocurre al usar SINIM descargando su catalogo de manera unitaria.

**Fuente:** https://datos.sinim.gov.cl/datos_municipales.php

---

### Instalación

#### Paso 1: Instalar Python
Asegúrate de tener **Python 3.8 o superior** instalado. 

Verifica en CMD:
```cmd
python --version
```

#### Paso 2: Instalar librerías requeridas

Abre CMD en la carpeta del proyecto y ejecuta:

```cmd
pip install -r requirements.txt
```

Las librerías que se instalarán son:
- **playwright** - Para navegar y extraer datos automáticamente
- **pandas** - Para procesar y guardar datos
- **customtkinter** - Para la interfaz gráfica (opcional)

#### Paso 3: Preparar Playwright

Ejecuta una sola vez:
```cmd
playwright install
```

---

### Cómo usar en CMD

#### Formato básico:
```cmd
python src/scraper.py "ID - NOMBRE_VARIABLE" año_inicio,año_fin
```

#### Ejemplos:

**Ejemplo 1:** Extraer variable 618 (GASTO TOTAL) para 2023 a 2025
```cmd
python src/scraper.py "618 - GASTO TOTAL" 2023,2025
```

**Ejemplo 2:** Extraer variable 100 (POBLACIÓN) para 2020 a 2024
```cmd
python src/scraper.py "100 - POBLACIÓN" 2020,2024
```

**Ejemplo 3:** Extraer múltiples variables en un comando
```cmd
python src/scraper.py "618 - GASTO TOTAL,100 - POBLACIÓN" 2023,2025
```

---

### Datos de salida

#### Ubicación
Los archivos se guardan en la **carpeta raíz del proyecto**

#### Nombre del archivo
- `[ID]_[año_inicio]_[año_fin].xlsx`

**Ejemplo:** `618_2023_2025.xlsx`

#### Contenido del archivo (columnas)
| Columna | Descripción |
|---------|------------|
| Año | Año del dato (ej: 2023) |
| Region | Región del municipio |
| Cod_Territorial | Código del municipio |
| Municipio | Nombre del municipio |
| [ID] | Valor de la variable (ej: 618) |

#### Ejemplo de datos:
```
Año  | Region        | Cod_Territorial | Municipio      | 618
-----|---------------|-----------------|----------------|-------
2023 | METROPOLITANA | 13001           | Santiago       | 15000.50
2023 | METROPOLITANA | 13002           | Puente Alto    | 8500.25
2024 | METROPOLITANA | 13001           | Santiago       | 16200.75
```

---

### Requisitos técnicos

- **Conexión a Internet:** Necesaria para acceder al sitio SINIM
- **Tiempo de ejecución:** Varía según cantidad de variables y años (típicamente 30 segundos - 2 minutos por variable)
- **Espacio en disco:** Mínimo 100 MB libres

---

### Solución de problemas

#### Error: "No module named 'playwright'"
**Solución:**
```cmd
pip install playwright
playwright install
```

#### Error: "El sitio no responde"
- Verifica tu conexión a Internet
- Intenta de nuevo en unos minutos
- El sitio SINIM podría estar en mantenimiento

#### El archivo Excel se guarda en lugar inesperado
- Los archivos se guardan donde ejecutes el comando
- Navega a la carpeta del proyecto antes de ejecutar:
```cmd
cd e:\scraper-sinim-2026
python src/scraper.py "618 - GASTO TOTAL" 2023,2025
```

---

### Notas útiles

- Los IDs y nombres de variables se encuentran en: `docs/indice_15_01_2026.csv`
- Puedes usar rangos de años (ej: `2020,2024`) o años específicos (ej: `2023,2023,2024`)
- El scraper procesa **todos los municipios de Chile** automáticamente (16 regiones)
- Los datos se limpian automáticamente (sin símbolos %, puntos innecesarios, etc.)


