import math

def hex_to_rgb(hex_color):
    """Convert hex color string to RGB tuple (0-1 range)."""
    hex_color = hex_color.lstrip('#')
    rgb = tuple(int(hex_color[i:i+2], 16) for i in (0, 2, 4))
    return tuple(x/255 for x in rgb)

def rgb_to_hex(rgb):
    """Convert RGB tuple (0-1 range) to hex color string."""
    rgb_int = tuple(int(x * 255) for x in rgb)
    return '#{:02x}{:02x}{:02x}'.format(*rgb_int)

def rgb_to_lab_lightness(rgb):
    """Convert RGB to L* (lightness) value in CIELAB color space."""
    # Convert RGB to XYZ
    r, g, b = rgb
    r = _gamma_expand(r)
    g = _gamma_expand(g)
    b = _gamma_expand(b)
    
    x = 0.4124564 * r + 0.3575761 * g + 0.1804375 * b
    y = 0.2126729 * r + 0.7151522 * g + 0.0721750 * b
    z = 0.0193339 * r + 0.1191920 * g + 0.9503041 * b
    
    # Convert Y to L* (lightness)
    y = y / 1.0  # Normalize to reference white
    if y > 0.008856:
        return 116 * math.pow(y, 1/3) - 16
    else:
        return 903.3 * y

def _gamma_expand(value):
    """Apply gamma expansion (inverse of gamma correction)."""
    if value <= 0.04045:
        return value / 12.92
    else:
        return math.pow((value + 0.055) / 1.055, 2.4)

def _gamma_compress(value):
    """Apply gamma compression (gamma correction)."""
    if value <= 0.0031308:
        return value * 12.92
    else:
        return 1.055 * math.pow(value, 1/2.4) - 0.055

def lab_lightness_to_rgb(l_value):
    """Convert L* (lightness) value to RGB."""
    # Convert L* to Y
    if l_value > 8:
        y = math.pow((l_value + 16) / 116, 3)
    else:
        y = l_value / 903.3
    
    # Since we're dealing with grayscale, R=G=B
    # We need to reverse the gamma correction
    gray = _gamma_compress(y)
    return (gray, gray, gray)

def generate_grayscale_colors(darkest, brightest, n):
    """
    Generate n perceptually uniform grayscale colors between darkest and brightest.
    
    Args:
        darkest (str): Hex color code for darkest gray
        brightest (str): Hex color code for brightest gray
        n (int): Number of colors to generate
        
    Returns:
        list: List of hex color codes
    """
    if n < 2:
        raise ValueError("n must be at least 2")
    
    # Convert hex colors to RGB
    dark_rgb = hex_to_rgb(darkest)
    bright_rgb = hex_to_rgb(brightest)
    
    # Convert to L* values
    dark_l = rgb_to_lab_lightness(dark_rgb)
    bright_l = rgb_to_lab_lightness(bright_rgb)
    
    # Generate evenly spaced L* values
    l_values = [dark_l + (bright_l - dark_l) * i / (n - 1) for i in range(n)]
    
    # Convert back to RGB and then hex
    return [rgb_to_hex(lab_lightness_to_rgb(l)) for l in l_values]

# Example usage
if __name__ == "__main__":
    colors = generate_grayscale_colors("#848484", "#E5E5E5", 5)
    print("Generated colors:", colors)
    
    # Print a simple visualization
    for color in colors:
        print(f"Color: {color}")
