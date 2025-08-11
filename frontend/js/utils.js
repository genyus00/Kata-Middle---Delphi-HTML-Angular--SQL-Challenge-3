// utils.js
import { API_URL, AUTH_HEADER } from './config.js';

export function cancelarFormulario(formId) {
    const form = document.getElementById(formId);
    if (!form) {
        console.error(`Formulario con ID ${formId} no encontrado`);
        return;
    }
    form.reset();

    const hiddenInputs = form.querySelectorAll("input[type='hidden']");
    hiddenInputs.forEach(input => input.value = "");

    const submitBtn = form.querySelector("button[type='submit']");
    if (submitBtn) submitBtn.textContent = "Guardar";
}

export async function apiRequest(endpoint, method = "GET", body = null) {
    const options = {
        method,
        headers: {
            "Content-Type": "application/json",
            "Authorization": AUTH_HEADER
        }
    };
    if (body) options.body = JSON.stringify(body);

    try {
        const res = await fetch(`${API_URL}${endpoint}`, options);
        if (!res.ok) throw new Error(`Error HTTP ${res.status}`);
        return await res.json();
    } catch (error) {
        console.error("Error en la petición:", error);
        return { CodError: 1, ErrorMsg: error.message };
    }
}
