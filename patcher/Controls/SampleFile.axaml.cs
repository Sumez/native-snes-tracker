using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Interactivity;
using Avalonia.Media;
using Avalonia.Threading;
using System;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;

namespace Patcher.Controls;

public partial class SampleFile : UserControl
{
	private static double SemitoneFactor = Math.Pow(2, (double)1 / 2); // Jump up or down half an octave every playback
	private CancellationTokenSource _cancellationTokenSource;

	public double CompressedSampleRate { get; private set; } = 32000;
	public double PreviewSampleRate { get; set; } = 32000;

	public ViewModels.SampleFile? File { get { return DataContext as ViewModels.SampleFile; } set { DataContext = value; } }

	public SampleFile()
    {
		InitializeComponent();
		Bitrate.ItemsSource = new[] { "36", "54", "72", "108", "144", "198" };
		Bitrate.MinimumPrefixLength = 0;
		Bitrate.KeyDown += Bitrate_KeyDown;
		Bitrate.TextChanged += Bitrate_TextChanged;
	}

	private void Bitrate_TextChanged(object? sender, TextChangedEventArgs e)
	{
		if (File?.UncompressedSourceFile == null) return;
		if (_cancellationTokenSource != null && _cancellationTokenSource.IsCancellationRequested) _cancellationTokenSource.Cancel();

		Bitrate.Classes.Set("invalid", File.CompressionBitRate < 36 || File.CompressionBitRate > 256);
		if (File.CompressionBitRate < 36 || File.CompressionBitRate > 256) return;

		var sampleRate = File.CompressionBitRate * 16000 / 72; // kbit to BRR samples. 144kbit = 32000hz

		_cancellationTokenSource = new CancellationTokenSource();
		var file = File; // Can't access datacontext property from async thread
		Task.Run(async () =>
		{
			await Task.Delay(400, _cancellationTokenSource.Token);
			file.Resample(sampleRate, _cancellationTokenSource.Token);
			if (_cancellationTokenSource.Token.IsCancellationRequested) return;
			PreviewSampleRate = CompressedSampleRate = sampleRate;

		}, _cancellationTokenSource.Token);
	}

	private void Bitrate_KeyDown(object? sender, KeyEventArgs e) {
		if (e.KeySymbol != null && !Regex.IsMatch(e.KeySymbol, @"[0-9]")) e.Handled = true;
	}

	private Brewsic.Playback.PreviewPlayer? GetPreviewPlayer()
	{
		var topLevel = TopLevel.GetTopLevel(this) as Window;
		var mainWindow = topLevel as MainWindow;
		if (mainWindow == null) mainWindow = topLevel?.Owner as MainWindow;
		return mainWindow?.PreviewPlayer;
	}
	private void Play(object? sender, RoutedEventArgs e)
    {
		var brrSample = File?.BrrSample;
		if (brrSample != null)
		{
			GetPreviewPlayer()?.PlayBrr(brrSample.SampleData, (int)PreviewSampleRate, brrSample.LoopStart);
			PreviewSampleRate /= SemitoneFactor;
			if (PreviewSampleRate < 7900) PreviewSampleRate = 64000;
		}
	}

	private void PlayUncompressed(object? sender, RoutedEventArgs e)
	{
		if (File?.UncompressedSourceFile != null)
		{
			GetPreviewPlayer()?.PlayFile(File?.UncompressedSourceFile);
			PreviewSampleRate = CompressedSampleRate;
		}
	}

}