// caficultores.js
import { apiRequest, cancelarFormulario, manejarError } from './utils.js';

// ===== CRUD CAFICULTORES =====
export async function cargarCaficultores() {
    const result = await apiRequest("/GetCaficultores");
    const tbody = document.querySelector("#caficultoresTable tbody");
    tbody.innerHTML = "";

    if (manejarError(result)) return;

    // Si no hay error, procesamos normalmente
    if (Array.isArray(result.data)) {
        result.data.forEach(c => {
            const tr = document.createElement("tr");
            tr.innerHTML = `
                <td>${c.id}</td>
                <td>${c.nombre}</td>
                <td>${c.identificacion}</td>
                <td>${c.ciudad}</td>
                <td>${c.tipo_producto}</td>
                <td>
                    <button class="editar-btn">Editar</button>
                    <button class="eliminar-btn">Eliminar</button>
                </td>
            `;

            tr.querySelector(".editar-btn").addEventListener("click", () => {
                editarCaficultor(c.id, c.nombre, c.identificacion, c.ciudad, c.tipo_producto);
            });
            tr.querySelector(".eliminar-btn").addEventListener("click", () => {
                eliminarCaficultor(c.id);
            });

            tbody.appendChild(tr);
        });
    }
}

export function editarCaficultor(id, nombre, identificacion, ciudad, tipo_producto) {
    document.getElementById("caficultorId").value = id;
    document.getElementById("nombre").value = nombre;
    document.getElementById("identificacion").value = identificacion;
    document.getElementById("ciudad").value = ciudad;
    document.getElementById("tipo_producto").value = tipo_producto;
    document.getElementById("btnGuardar").textContent = "Actualizar";
}

export async function eliminarCaficultor(id) {
    if (!confirm("¿Eliminar este caficultor?")) return;

    const result = await apiRequest(`/caficultor/${id}`, "DELETE");
    
    if (manejarError(result)) return;

    alert(result.message || "Operación completada");
    cargarCaficultores();
}

export function attachCaficultorEvents() {
    document.getElementById("caficultorForm").addEventListener("submit", async function(e) {
        e.preventDefault();

        const id = parseInt(document.getElementById("caficultorId").value, 10) || 0; // 0 si vacío
        const data = {
            nombre: document.getElementById("nombre").value,
            identificacion: document.getElementById("identificacion").value,
            ciudad: document.getElementById("ciudad").value,
            tipo_producto: document.getElementById("tipo_producto").value
        };

        const endpoint = id > 0 ? `/caficultor/${id}` : "/caficultor";
        const method = id > 0 ? "PUT" : "POST";
        const result = await apiRequest(endpoint, method, data);

        if (manejarError(result)) return;       
        
        alert(result.message || "Operación completada");
        cargarCaficultores();
        cancelarFormulario("caficultorForm");
    });

    // Asignar eventos a los botones cancelar
    document.getElementById("cancelarCaficultor").addEventListener("click", () => {
        cancelarFormulario("caficultorForm");
        document.getElementById("btnGuardar").textContent = "Guardar";
    });
}
