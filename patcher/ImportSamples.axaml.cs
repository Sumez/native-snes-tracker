using Avalonia.Controls;
using Avalonia.Interactivity;
using Patcher.ViewModels;
using System.Collections.Generic;
using System.Linq;

namespace Patcher;

public partial class ImportSamples : Window
{
	public IEnumerable<SampleFile> SamplesToAddToPatch { get; private set; } = new List<SampleFile>();
	public ImportSamples()
	{
		InitializeComponent();
		DataContext = _sampleFiles = new SampleFileCollection();
	}
	public ImportSamples(List<SampleFile> sampleFiles) : this()
	{
		DataContext = _sampleFiles = new SampleFileCollection(sampleFiles);
	}
	private readonly SampleFileCollection _sampleFiles;

	public void AddSampleFile(SampleFile sampleFile) => _sampleFiles.Add(sampleFile);

	public void AddSelectedSamples(object? sender, RoutedEventArgs args)
	{
		SamplesToAddToPatch = _sampleFiles.Where(s => s.Selected);
		Close();
	}
}