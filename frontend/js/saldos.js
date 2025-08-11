// saldos.js
import { apiRequest } from './utils.js';

// ===== CONSULTA SALDOS CAFICULTOR =====
export async function cargarSaldos() {
    document.getElementById("buscarCaficultorId_sm").value = "";
    const result = await apiRequest("/GetSaldoMonedero/0");
    renderSaldos(result);
}

export async function buscarSaldo() {
    const idCaficultor = document.getElementById("buscarCaficultorId_sm").value.trim();

    if (!idCaficultor || isNaN(idCaficultor)) {
        alert("Ingrese un ID de caficultor válido (numérico)");
        return;
    }

    try {
        const result = await apiRequest(`/GetSaldoMonedero/${idCaficultor}`);
        renderSaldos(result);
    } catch (error) {
        alert("Error al buscar saldo: " + error.message);
    }
}

function renderSaldos(result) {
    const tbody = document.querySelector("#saldosTable tbody");
    tbody.innerHTML = "";

    if (result.CodError === 0 && Array.isArray(result.data)) {
        result.data.forEach(s => {
            const tr = document.createElement("tr");
            tr.innerHTML = `
                <td>${s.id_caficultor}</td>
                <td>${Number(s.saldo).toLocaleString('es-CO', { style: 'currency', currency: 'COP' })}</td>                
            `;
            tbody.appendChild(tr);
        });
    } else {
        alert(result.ErrorMsg || "No se encontraron saldos.");
    }
}

export function attachSaldoEvents() {
    document.getElementById("btnBuscar_sm").addEventListener("click", () => {
        buscarSaldo();        
    });       

    document.getElementById("btnVerTodo_sm").addEventListener("click", () => {
        cargarSaldos();        
    });  
}