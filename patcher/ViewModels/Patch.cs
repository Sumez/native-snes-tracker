using System.ComponentModel;
using System.IO;

namespace Patcher.ViewModels;
public class Patch : INotifyPropertyChanged
{
	private string? _romFilePath;
	public string? RomFilePath { get => _romFilePath; set { 
			_romFilePath = value; 
			if (PropertyChanged != null)
			{
				PropertyChanged(this, new PropertyChangedEventArgs(nameof(RomFilePath)));
				PropertyChanged(this, new PropertyChangedEventArgs(nameof(RomFileName)));
			}
		} }
	public string? RomFileName { get => Path.GetFileName(_romFilePath); }
	public SampleFileCollection AvailableSamples { get; } = new();

	public event PropertyChangedEventHandler? PropertyChanged;
}
