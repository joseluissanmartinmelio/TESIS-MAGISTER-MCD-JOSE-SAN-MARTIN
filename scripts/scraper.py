import sys
import pandas as pd
from playwright.sync_api import sync_playwright
import time

REGIONES = [
    "TARAPACÁ", "ANTOFAGASTA", "ATACAMA", "COQUIMBO", "VALPARAÍSO",
    "DEL LIBERTADOR GRAL. BERNARDO O´HIGGINS", "DEL MAULE", "DEL BIOBIO",
    "DE LA ARAUCANÍA", "DE LOS LAGOS", "AYSÉN DEL GRAL. CARLOS IBÁÑEZ DEL CAMPO",
    "MAGALLANES Y DE LA ANTÁRTICA CHILENA", "METROPOLITANA", "LOS RíOS",
    "ARICA Y PARINACOTA", "ÑUBLE"
]

def ejecutar_extraccion_panel(lista_variables, lista_anios):
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page()

        for variable_full in lista_variables:
            # --- MODIFICACIÓN: Extraer solo el ID ---
            # Si el usuario ingresa "618 - GASTO TOTAL", variable_id será "618"
            variable_id = variable_full.split("-")[0].strip()
            
            datos_panel = []
            print(f"\n=== Procesando variable ID: {variable_id} ===")
            
            for region in REGIONES:
                for anio in lista_anios:
                    page.goto("https://datos.sinim.gov.cl/datos_municipales.php")

                    time.sleep(1) 

                    # Selección de Área y Subárea
                    page.get_by_role("link", name="Área").click()
                    page.locator("#dato_area_chzn").get_by_role("textbox").click()
                    page.locator("#dato_area_chzn_o_1").click()

                    page.locator("#dato_subarea_chzn").get_by_role("textbox").click()
                    page.locator("#dato_subarea_chzn_o_1").click()

                    # Selección de Variable
                    page.get_by_role("link", name="Variables e Indicadores").click()
                    input_var = page.locator("#variables_chzn").get_by_role("textbox")
                    input_var.click()
                    time.sleep(0.5)
                    # Se usa el nombre completo para la búsqueda en la web
                    input_var.fill(variable_full) 
                    page.keyboard.press("Space")
                    page.keyboard.press("Enter")

                    # Selección de Año
                    page.get_by_role("link", name="Año").click()
                    input_anio = page.locator("#periodos_chzn").get_by_role("textbox")
                    input_anio.click()
                    input_anio.fill(anio)
                    page.keyboard.press("Space")
                    page.keyboard.press("Enter")

                    # Selección de Región
                    page.get_by_role("link", name="Región").click()
                    input_reg = page.locator("#regiones_chzn").get_by_role("textbox")
                    input_reg.click()
                    input_reg.fill(region)
                    page.keyboard.press("Space")
                    page.keyboard.press("Enter")

                    # Selección de Municipios (TODOS)
                    page.get_by_role("link", name="Municipio").click()
                    input_mun = page.locator("#municipios_chzn").get_by_role("textbox")
                    input_mun.click()
                    input_mun.fill("TODOS")
                    page.keyboard.press("Space")
                    page.keyboard.press("Enter")

                    page.get_by_role("link", name="Ver", exact=True).click()
                    time.sleep(0.5)

                    time.sleep(1)
                    
                    try:
                        page.wait_for_selector("#resultados", state="visible", timeout=15000)

                        while page.locator("#next").is_visible():
                            page.locator("#next").click()
                            time.sleep(1)

                        nombres = page.locator(".resultado_municipios .item_large").all_inner_texts()
                        codigos = page.locator(".resultado_municipios span[class^='cod_mun_']").all_inner_texts()
                        valores_raw = page.locator("#valores .item_dato").all_inner_texts()

                        for n, c, v in zip(nombres, codigos, valores_raw):
                            valor_limpio = v.replace("%", "").strip().replace(".", "").replace(",", ".")
                            datos_panel.append({
                                "Año": anio,
                                "Region": region,
                                "Cod_Territorial": c.strip(),
                                "Municipio": n.strip(),
                                f"{variable_id}": valor_limpio # Se guarda solo el ID como cabecera
                            })
                        print(f"  {region} - {anio}")

                    except:
                        print(f"  error: no se cargaron datos para {region} en {anio}")

            # Exportación a XLSX
            if datos_panel:
                df = pd.DataFrame(datos_panel)
                # El nombre del archivo ahora solo usará el ID
                nombre_archivo = f"{variable_id}_{lista_anios[0]}_{lista_anios[-1]}.xlsx"
                df.to_excel(nombre_archivo, index=False)
                print(f"Archivo generado: {nombre_archivo} ({len(datos_panel)} registros)")
        
        browser.close()
        print("\n=== Proceso completado ===")

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Uso: python scraper.py \"ID - NOMBRE\" 2020,2024")
    else:
        var_input = sys.argv[1]
        vars_lista = [v.strip() for v in var_input.split(",")]
        
        anios_input = sys.argv[2].split(",")
        if len(anios_input) == 2 and anios_input[0].isdigit() and anios_input[1].isdigit():
            anio_inicio = int(anios_input[0].strip())
            anio_fin = int(anios_input[1].strip())
            anios_lista = [str(anio) for anio in range(anio_inicio, anio_fin + 1)]
        else:
            anios_lista = [a.strip() for a in anios_input]
        
        ejecutar_extraccion_panel(vars_lista, anios_lista)