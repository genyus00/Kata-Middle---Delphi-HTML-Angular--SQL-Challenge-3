// abonos.js
import { apiRequest, cancelarFormulario, manejarError } from './utils.js';

// ===== CRUD ABONOS MONEDEROS =====
// Función para cargar todos los abonos al iniciar o refrescar
export async function cargarAbonos() {
    document.getElementById("buscarCaficultorId_am").value = "";
    document.getElementById("fechaInicio_am").value = "";
    document.getElementById("fechaFin_am").value = "";
    const result = await apiRequest("/GetAbonosMonedero");
    
    if (manejarError(result)) return;

    renderAbonos(result);
}

// Función para editar abono, expuesta globalmente para el onclick
export function editarAbono(id, valor, fecha, id_caficultor) {
    document.getElementById("abonoId").value = id;
    document.getElementById("fecha").value = fecha;
    document.getElementById("valor").value = valor;
    document.getElementById("id_caficultor_am").value = id_caficultor;
    document.getElementById("btnGuardar_am").textContent = "Actualizar";        
}


// Función para eliminar abono, expuesta globalmente para el onclick
export async function eliminarAbono(id) {
    if (!confirm("¿Eliminar este abono?")) return;
    const result = await apiRequest(`/AbonoMonedero/${id}`, "DELETE");

    if (manejarError(result)) return;     
    
    alert(result.message || "Operación completada");
    cargarAbonos();
}


// Función para buscar abonos filtrados
export async function buscarAbonos() {
    const idCaficultor = document.getElementById("buscarCaficultorId_am").value.trim();
    const fechaInicio = document.getElementById("fechaInicio_am").value.trim();
    const fechaFin = document.getElementById("fechaFin_am").value.trim();

    // Normalizamos valores para DataSnap
    const id = idCaficultor || "0";
    const inicio = fechaInicio || "";
    const fin = fechaFin || "";

    // Armamos URL estilo DataSnap REST
    const url = `/GetAbonosFiltrados/${encodeURIComponent(id)}/${encodeURIComponent(inicio)}/${encodeURIComponent(fin)}`;

    const result = await apiRequest(url);

    if (manejarError(result)) return;

    renderAbonos(result);
}

// Función para renderizar la tabla de abonos
function renderAbonos(result) {
    const tbody = document.querySelector("#abonosTable tbody");
    tbody.innerHTML = "";

    if (Array.isArray(result.data) && result.data.length > 0) {
        result.data.forEach(p => {
            const tr = document.createElement("tr");
            tr.innerHTML = `
                <td>${p.id_caficultor}</td>
                <td>${Number(p.valor).toLocaleString('es-CO', { style: 'currency', currency: 'COP' })}</td>
                <td>${p.fecha}</td>
                <td>
                    <button class="editar-btn">Editar</button>
                    <button class="eliminar-btn">Eliminar</button>
                </td>
            `;

            tr.querySelector(".editar-btn").addEventListener("click", () => {
                editarAbono(p.id, p.valor, p.fecha, p.id_caficultor);
            });
            tr.querySelector(".eliminar-btn").addEventListener("click", () => {
                eliminarAbono(p.id);
            });
            tbody.appendChild(tr);
            
        });
    } else {
        alert("No se encontraron abonos para ese caficultor");
    }
}

export function attachAbonosEvents() {
    document.getElementById("abonoForm").addEventListener("submit", async function(e) {
        e.preventDefault();
        const id = parseInt(document.getElementById("abonoId").value, 10) || 0; // 0 si vacío
        const data = {
            id_caficultor: parseInt(document.getElementById("id_caficultor_am").value, 10),
            valor: document.getElementById("valor").value,
            fecha: document.getElementById("fecha").value
        };

        const endpoint = id > 0 ? `/AbonoMonedero/${id}` : "/RegistrarAbono"; //AbonoMonedero
        const method = id > 0 ? "PUT" : "POST";
        const result = await apiRequest(endpoint, method, data);

        if (manejarError(result)) return; 
        
        alert(result.message || "Operación completada");

        cargarAbonos();
        cancelarFormulario("abonoForm");
    });

    document.getElementById("cancelarAbono").addEventListener("click", () => {
        cancelarFormulario("abonoForm");
        document.getElementById("btnGuardar_am").textContent = "Guardar";
    });

    document.getElementById("btnBuscar_am").addEventListener("click", () => {
        buscarAbonos();        
    });       

    document.getElementById("btnVerTodo_am").addEventListener("click", () => {
        cargarAbonos();        
    });       
}

