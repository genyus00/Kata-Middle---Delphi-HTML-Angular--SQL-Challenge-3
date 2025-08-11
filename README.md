# Kata-Middle---Delphi-HTML-Angular--SQL-Challenge-3
- Reto Fullstack Escalable Externo
- Registrar caficultores con: nombre, identificación, ciudad y tipo de producto.
- Registrar abonos a monedero con validaciones básicas.
- Consultar saldo del monedero por caficultor.
- Mostrar productos bancarios asociados.
- Visualizar tablas con abonos y caficultores.
- Filtros por fechas para abonos.
- CRUD básico para todas las entidades.
- Uso de procedimientos almacenados para registrar abonos, consultar saldos y consultar productos.
- Separación frontend (HTML+JS) y backend (Delphi + BD).

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
  usuario1=clave1
  usuario2=clave2
  usuario3=clave3

- **DBConex.ini** — Parámetros de conexión a la base de datos SQL Server.

  ```ini
  [MSQLServer]
  Server=DESKTOP-4AAQJLB\SQLEXPRESS
  DataBase=DBKATA
  User=sa
  Password=Asdf1234$

- **Instrucciones de ejecución**
   - El ejecutable del servidor se llama sa_ServidorApp.exe.
   - Para iniciar el servidor, simplemente haz doble clic en el archivo .exe.
   - Al iniciar, el servidor carga la configuración desde los archivos .ini.
   - Desde ese momento, podrás iniciar o detener el servidor según sea necesario.
   - El servidor expone los métodos API para ser consumidos desde el frontend o herramientas externas.

**Nota adicional**
   - El backend también cuenta con una versión en .dll para desplegar en IIS, que no forma parte de esta guía de ejecución.

## 3. Pruebas de API con Postman
Para facilitar las pruebas de los métodos expuestos por el servidor, se incluyen colecciones Postman dentro de la carpeta json:

   - **DataSnap - TServerMethods1 API (Auth).postman_collection v2.json**
     Colección para probar métodos relacionados con autenticación y funcionalidades principales.

   - **Pruebas GetAbonosFiltrados - GET.postman_collection.json**
     Colección para probar específicamente la consulta de abonos filtrados por parámetros.

  - Importa estas colecciones en Postman para ejecutar y verificar los endpoints.

## 4. Frontend (HTML + JavaScript)
**Requisitos**
- Navegador web moderno (Chrome, Firefox, Edge).

**Pasos**

**1.** Abre el archivo index.html directamente en el navegador.

**2.** Asegúrate de que el backend esté corriendo para que las llamadas a la API funcionen correctamente.

**3.** El frontend realiza llamadas AJAX hacia el servidor para mostrar datos y realizar operaciones.

Si tienes dudas o problemas durante la ejecución, consulta los logs del backend o la consola del navegador para identificar errores.
