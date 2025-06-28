// Función para generar un color aleatorio pero determinístico basado en un número
function generateColorFromNumber(num: number): string {
    // Función hash mucho más agresiva para máxima distribución
    let hash1 = num;
    
    // Primera transformación: usar diferentes operaciones según la posición del bit
    hash1 = hash1 ^ (hash1 << 13);
    hash1 = hash1 ^ (hash1 >>> 17);
    hash1 = hash1 ^ (hash1 << 5);
    hash1 = Math.abs(hash1) % 982451653; // Número primo grande
    
    // Segunda transformación: multiplicar por primos y rotar bits
    let hash2 = num * 1597334677;
    hash2 = ((hash2 << 15) | (hash2 >>> 17)) ^ num;
    hash2 = hash2 * 2654435761;
    hash2 = Math.abs(hash2) % 1000000007; // Otro primo grande
    
    // Tercera transformación: usar logaritmo para separar potencias de 2
    const logBase = Math.log2(num);
    let hash3 = Math.floor(logBase * 31415926) ^ (num >>> 8);
    hash3 = hash3 * 48271; // Generador congruencial lineal
    hash3 = Math.abs(hash3) % 2147483647;
    
    // Combinar los tres hashes con XOR y rotación
    const combinedHash = hash1 ^ hash2 ^ hash3;
    const finalHash = ((combinedHash << 7) | (combinedHash >>> 25)) * 69069;
    
    // Evitar rangos de colores ya usados
    const avoidRanges = [
        [10, 40],   // naranjas/amarillos
        [140, 170], // verdes
        [190, 220], // azules/cianes  
        [270, 310], // púrpuras/magentas
        [50, 80]    // amarillos/verdes claros
    ];
    
    let hue: number;
    let attempts = 0;
    do {
        // Usar diferentes multiplicadores primos para cada intento
        const primes = [73, 137, 211, 307, 401, 503, 601, 701, 809, 907];
        const multiplier = primes[attempts % primes.length];
        const seed = Math.abs(finalHash + attempts * multiplier + logBase * 191);
        hue = seed % 360;
        attempts++;
    } while (attempts < 20 && avoidRanges.some(([min, max]) => hue >= min && hue <= max));
    
    // Saturación y luminosidad más variables usando diferentes semillas
    const satSeed = Math.abs(finalHash * 17 + num * 23);
    const lightSeed = Math.abs(finalHash * 29 + num * 37);
    
    const saturation = 65 + (satSeed % 30); // Entre 65% y 95%
    const lightness = 45 + (lightSeed % 25);  // Entre 45% y 70%
    
    return `hsl(${hue}, ${saturation}%, ${lightness}%)`;
}

export function numberToColor(num: number) {
    switch (num) {
        case 2: return "#0083bb";
        case 4: return "#fc7f40";
        case 8: return "#e6538a";
        case 16: return "#058555";
        case 32: return "#b82bfa";
        case 64: return "#a24e78";
        case 128: return "#b9c508";
        default: 
            // Para números >= 256, generar colores aleatorios determinísticos
            if (num >= 256) {
                return generateColorFromNumber(num);
            }
            return "black";
    }
}

export function delay(milliseconds: number) {
    return new Promise(resolve => setTimeout(resolve, milliseconds));
}