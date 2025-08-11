// main.js
import * as caficultores from './caficultores.js';
import * as productos from './productos.js';
import * as abonos from './abonos.js';
import * as saldos from './saldos.js';

document.addEventListener("DOMContentLoaded", () => {
    // Cargar datos iniciales
    caficultores.cargarCaficultores();
    caficultores.attachCaficultorEvents();

    productos.cargarProductos();
    productos.attachProductoEvents();

    abonos.cargarAbonos();  
    abonos.attachAbonosEvents();

    saldos.attachSaldoEvents();
});
