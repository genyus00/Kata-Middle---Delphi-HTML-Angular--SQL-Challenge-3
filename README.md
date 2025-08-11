# Kata-Middle---Delphi-HTML-Angular--SQL-Challenge-3
Reto Fullstack Escalable Externo

# README Técnico - Instrucciones Básicas de Ejecución

---

## 1. Configuración de la Base de Datos

### Requisitos
- Microsoft SQL Server 2008 R2 (o superior) o equivalente compatible.
- SQL Server Management Studio (SSMS) o herramienta similar para ejecutar scripts.

### Pasos

1. **Crear la base de datos**  
   Crea una base de datos nueva para el proyecto (por ejemplo, `BancoCafetero`).

2. **Ejecutar los scripts de creación**  
   Ejecuta los scripts SQL proporcionados en la carpeta `/scripts` para:
   - Crear tablas (`CAFICULTORES`, `PRODUCTOS_CLIENTE`, `ABONOS_MONEDERO`).
   - Crear vistas (por ejemplo, `SALDO_MONEDERO`).
   - Crear procedimientos almacenados (`sp_ConsultarProductos`, `sp_ConsultarSaldo`, etc.).

3. **Insertar datos de prueba**  
   Ejecuta el script de carga de datos para insertar registros de prueba y reiniciar los contadores de identidad.

---

## 2. Backend (Delphi / Embarcadero)

### Archivos de configuración

El servidor utiliza dos archivos `.ini` para su parametrización:

- **Auth.ini** — Configuración de usuarios autorizados para la aplicación.

  ```ini
  [Users]
  genyus00=010577
  otroUsuario=suClave
