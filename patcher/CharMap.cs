using System.Text;

namespace Patcher;
public static class CharMap
{
	public static char FromByte(int charByte)
	{
		if (charByte >= 0x00 && charByte <= 0x19) return Encoding.ASCII.GetChars(new[] { (byte)(charByte + 65 - 0x00) })[0];
		if (charByte >= 0x20 && charByte <= 0x39) return Encoding.ASCII.GetChars(new[] { (byte)(charByte + 97 - 0x20) })[0];
		if (charByte >= 0x40 && charByte <= 0x49) return Encoding.ASCII.GetChars(new[] { (byte)(charByte + 48 - 0x40) })[0];
		return charByte switch
		{
			0x1A => '!',
			0x1B => '?',
			0x1C => '#',
			0x3A => '.',
			0x3B => '-',
			0x3E => '~',
			0x3F => ' ',
			_ => ' ',
		};
	}
	public static byte FromChar(char character)
	{
		var asciiByte = Encoding.ASCII.GetBytes(new[] { character })[0];
		if (asciiByte >= 65 && asciiByte <= 90) return (byte)(asciiByte - 65 + 0x00);
		if (asciiByte >= 97 && asciiByte <= 122) return (byte)(asciiByte - 97 + 0x20);
		if (asciiByte >= 48 && asciiByte <= 57) return (byte)(asciiByte - 48 + 0x40);
		return character switch
		{
			'!' => 0x1A,
			'?' => 0x1B,
			'#' => 0x1C,
			'.' => 0x3A,
			'-' => 0x3B,
			'~' => 0x3E,
			' ' => 0x3F,
			_ => 0x3F,
		};
	}
	public static byte[] FromString(string value)
	{
		var bytes = new byte[value.Length];
		for (var i = 0; i < value.Length; i++) bytes[i] = FromChar(value[i]);
		return bytes;
	}
}
