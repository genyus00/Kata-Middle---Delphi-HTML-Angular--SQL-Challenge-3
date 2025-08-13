// saldos.js
import { apiRequest, manejarError } from './utils.js';

// ===== CONSULTA SALDOS CAFICULTOR =====
export async function cargarSaldos() {
    document.getElementById("buscarCaficultorId_sm").value = "";
    const result = await apiRequest("/GetSaldoMonedero/0");

    if (manejarError(result)) return;

    renderSaldos(result);
}

export async function buscarSaldo() {
    const idCaficultor = document.getElementById("buscarCaficultorId_sm").value.trim();

    if (!idCaficultor || isNaN(idCaficultor)) {
        alert("Ingrese un ID de caficultor válido (numérico)");
        return;
    }

    const result = await apiRequest(`/GetSaldoMonedero/${idCaficultor}`);
    
    if (manejarError(result)) return;

    renderSaldos(result);
}

function renderSaldos(result) {
    const tbody = document.querySelector("#saldosTable tbody");
    tbody.innerHTML = "";

    if (Array.isArray(result.data) && result.data.length > 0) {
        result.data.forEach(s => {
            const tr = document.createElement("tr");
            tr.innerHTML = `
                <td>${s.id_caficultor}</td>
                <td>${Number(s.saldo).toLocaleString('es-CO', { style: 'currency', currency: 'COP' })}</td>                
            `;
            tbody.appendChild(tr);
        });
    } else {
        alert("No se encontraron saldos.");
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