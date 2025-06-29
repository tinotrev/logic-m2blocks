function generateColorFromNumber(num: number): string {
    let hash1 = num;
    
    hash1 = hash1 ^ (hash1 << 13);
    hash1 = hash1 ^ (hash1 >>> 17);
    hash1 = hash1 ^ (hash1 << 5);
    hash1 = Math.abs(hash1) % 982451653;
    
    let hash2 = num * 1597334677;
    hash2 = ((hash2 << 15) | (hash2 >>> 17)) ^ num;
    hash2 = hash2 * 2654435761;
    hash2 = Math.abs(hash2) % 1000000007;
    
    const logBase = Math.log2(num);
    let hash3 = Math.floor(logBase * 31415926) ^ (num >>> 8);
    hash3 = hash3 * 48271;
    hash3 = Math.abs(hash3) % 2147483647;
    
    const combinedHash = hash1 ^ hash2 ^ hash3;
    const finalHash = ((combinedHash << 7) | (combinedHash >>> 25)) * 69069;
    
    const avoidRanges = [
        [10, 40],
        [140, 170],
        [190, 220], 
        [270, 310],
        [50, 80]
    ];
    
    let hue: number;
    let attempts = 0;
    do {
        const primes = [73, 137, 211, 307, 401, 503, 601, 701, 809, 907];
        const multiplier = primes[attempts % primes.length];
        const seed = Math.abs(finalHash + attempts * multiplier + logBase * 191);
        hue = seed % 360;
        attempts++;
    } while (attempts < 20 && avoidRanges.some(([min, max]) => hue >= min && hue <= max));
    
    const satSeed = Math.abs(finalHash * 17 + num * 23);
    const lightSeed = Math.abs(finalHash * 29 + num * 37);
    
    const saturation = 65 + (satSeed % 30);
    const lightness = 45 + (lightSeed % 25);
    
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
            if (num >= 256) {
                return generateColorFromNumber(num);
            }
            return "black";
    }
}

export function delay(milliseconds: number) {
    return new Promise(resolve => setTimeout(resolve, milliseconds));
}