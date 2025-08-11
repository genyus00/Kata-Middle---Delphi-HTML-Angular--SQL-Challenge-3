// productos.js
import { apiRequest, cancelarFormulario } from './utils.js';

// ===== CRUD PRODUCTOS =====
export async function cargarProductos() {
    document.getElementById("buscarCaficultorId_pc").value = "";
    const result = await apiRequest("/GetProductosCliente");
    renderProductos(result);
}

export function editarProducto(id, tipo_producto, numero_cuenta, id_caficultor) {
    document.getElementById("productoId").value = id;
    document.getElementById("tipo_producto_pc").value = tipo_producto;
    document.getElementById("numero_cuenta").value = numero_cuenta;
    document.getElementById("id_caficultor").value = id_caficultor;
    document.getElementById("btnGuardar_pc").textContent = "Actualizar";    
}

export async function eliminarProducto(id) {
    if (!confirm("¿Eliminar este producto?")) return;
    const result = await apiRequest(`/productocliente/${id}`, "DELETE");
    alert(result.message || result.ErrorMsg || "Operación completada");
    cargarProductos();
}

export async function buscarProducto() {
    const idCaficultor = document.getElementById("buscarCaficultorId_pc").value.trim();

    if (!idCaficultor) {
        alert("Ingrese un ID de caficultor");
        return;
    }

    const result = await apiRequest(`/GetProductosByCaficultor/${idCaficultor}`);
    renderProductos(result);
}

function renderProductos(result) {
    const tbody = document.querySelector("#productosTable tbody");
    tbody.innerHTML = "";

    if (result.CodError === 0 && Array.isArray(result.data)) {
        result.data.forEach(p => {
            const tr = document.createElement("tr");
            tr.innerHTML = `
                <!--<td>${p.id}</td>-->
                <td>${p.id_caficultor}</td>
                <td>${p.tipo_producto}</td>
                <td>${p.numero_cuenta}</td>
                <td>
                    <button class="editar-btn">Editar</button>
                    <button class="eliminar-btn">Eliminar</button>
                </td>
            `;

            tr.querySelector(".editar-btn").addEventListener("click", () => {
                editarProducto(p.id, p.tipo_producto, p.numero_cuenta, p.id_caficultor);
            });
            tr.querySelector(".eliminar-btn").addEventListener("click", () => {
                eliminarProducto(p.id);
            });

            tbody.appendChild(tr);
        });
    } else {
        alert(result.ErrorMsg || "No se encontraron productos para ese caficultor");
    }
}

export function attachProductoEvents() {
    document.getElementById("productoForm").addEventListener("submit", async function(e) {
        e.preventDefault();
        const id = parseInt(document.getElementById("productoId").value, 10) || 0; // 0 si vacío
        const data = {
            tipo_producto: document.getElementById("tipo_producto_pc").value,
            numero_cuenta: document.getElementById("numero_cuenta").value,
            id_caficultor: parseInt(document.getElementById("id_caficultor").value, 10)
        };

        const endpoint = id > 0 ? `/productocliente/${id}` : "/productocliente";
        const method = id > 0 ? "PUT" : "POST";
        const result = await apiRequest(endpoint, method, data);

        alert(result.message || result.ErrorMsg || "Operación completada");
        cargarProductos();
        cancelarFormulario("productoForm");    
    });

    document.getElementById("cancelarProducto").addEventListener("click", () => {
        cancelarFormulario("productoForm");
        document.getElementById("btnGuardar_pc").textContent = "Guardar";
    });   

    document.getElementById("btnBuscar_pc").addEventListener("click", () => {
        buscarProducto();        
    });       

    document.getElementById("btnVerTodo_pc").addEventListener("click", () => {
        cargarProductos();        
    });         
}

