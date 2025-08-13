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

export function manejarError(result) {
    if (result.CodError !== 0) {
        alert(`Error ${result.CodError}: ${result.ErrorMsg}`);
        return true; // hubo error
    }
    return false; // todo bien
}

export async function apiRequest(endpoint, method = "GET", body = null, timeout = 8000) {
    const options = {
        method,
        headers: {
            "Content-Type": "application/json",
            "Authorization": AUTH_HEADER
        }
    };
    if (body) options.body = JSON.stringify(body);

    // Función para manejar timeout con fetch
    const fetchWithTimeout = (url, options, ms) => {
        const controller = new AbortController();
        const id = setTimeout(() => controller.abort(), ms);
        return fetch(url, { ...options, signal: controller.signal })
            .finally(() => clearTimeout(id));
    };

    try {
        const res = await fetchWithTimeout(`${API_URL}${endpoint}`, options, timeout);

        if (!res.ok) {
            // Error HTTP real
            throw { http: true, status: res.status, message: `Error HTTP ${res.status}` };
        }

        return await res.json();

    } catch (error) {
        console.error("Error en la petición:", error);

        let errorMsg = "Error desconocido";
        let codError = 1;

        if (error.name === "AbortError") {
            // Timeout
            errorMsg = `Tiempo de espera agotado (${timeout / 1000}s)`;
            codError = 408; // HTTP 408 Request Timeout

        } else if (error.http) {
            // Error HTTP detectado manualmente
            errorMsg = error.message;
            codError = error.status;

        } else if (error instanceof TypeError && error.message.includes("Failed to fetch")) {
            // Servidor no disponible / DNS no resuelto
            errorMsg = "No se pudo conectar con el servidor (posiblemente apagado)";
            codError = 503; // Service Unavailable

        } else {
            // Otro tipo de error (desconocido)
            errorMsg = error.message || "Error de red inesperado";
            codError = 520; // 520 Unknown Error
        }

        return { CodError: codError, ErrorMsg: errorMsg };
    }
}

