using Brewsic;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Threading;
using System.Threading.Tasks;

namespace Patcher.ViewModels;

public class SampleFileCollection : ObservableCollection<SampleFile>
{
	public SampleFileCollection() : base() { }
	public SampleFileCollection(IEnumerable<SampleFile> sampleFiles) : base(sampleFiles) { }
	//public ObservableCollection<SampleFileData> SampleFiles { get; }
	//public SampleFileCollection(ObservableCollection<SampleFileData> samplefiles) => SampleFiles = samplefiles;
}

public class SampleFile : INotifyPropertyChanged
{
	public SampleFile(string name, BrrSample brrSample)
	{
		Name = name;
		BrrSample = brrSample;
	}

	public SampleFile(string name, string uncompressedSourceFile)
	{
		Name = name;
		UncompressedSourceFile = uncompressedSourceFile;
	}

	public void Resample(int sampleRate, CancellationToken? token = null)
	{
		BrrSample = null;
		if (PropertyChanged != null)
		{
			PropertyChanged(this, new PropertyChangedEventArgs(nameof(CompressedSize)));
			PropertyChanged(this, new PropertyChangedEventArgs(nameof(BrrSample)));
		}
		if (UncompressedSourceFile == null) return;

		AudioFile = AudioFile.LoadFromFile(UncompressedSourceFile, sampleRate, null, token);
		if (token?.IsCancellationRequested ?? true) return;
		GetBrrSampleFromAudioFile(token);
	}

	private void GetBrrSampleFromAudioFile(CancellationToken? token)
	{
		var sampleData = AudioFile.Sample.GetBrr(500, 0, (s) => { }, token);
		if (sampleData == null || (token?.IsCancellationRequested ?? true)) return;
		BrrSample = new BrrSample(sampleData);
		if (PropertyChanged != null)
		{
			PropertyChanged(this, new PropertyChangedEventArgs(nameof(CompressedSize)));
			PropertyChanged(this, new PropertyChangedEventArgs(nameof(BrrSample)));
		}
	}

	public int CompressionBitRate { get; set; } = 144;
	public string UncompressedSourceFile { get; private set; }

	public int PitchAdjust { get; set; } = 50;
	public bool Selected { get; set; } = true;
	public string Name { get; set; }
	public BrrSample? BrrSample { get; private set; }
	public string CompressedSize => BrrSample != null ? ((double)BrrSample.SampleData.Length / 1024).ToString("0.00 KB") : "compressing...";

	public AudioFile AudioFile { get; private set; }

	public event PropertyChangedEventHandler? PropertyChanged;
}
