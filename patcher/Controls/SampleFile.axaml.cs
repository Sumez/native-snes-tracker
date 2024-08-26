using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Interactivity;
using System;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;

namespace Patcher.Controls;

public partial class SampleFile : UserControl
{
	private static double SemitoneFactor = Math.Pow(2, (double)1 / 2); // Jump up or down half an octave every playback
	private CancellationTokenSource _cancellationTokenSource;

	public double PreviewSampleRate { get; set; } = 32000;

	public ViewModels.SampleFile? File { get { return DataContext as ViewModels.SampleFile; } set { DataContext = value; } }

	public SampleFile()
    {
		InitializeComponent();
		Bitrate.ItemsSource = new[] { "36", "54", "72", "108", "144", "198" };
		Bitrate.MinimumPrefixLength = 0;
		Bitrate.TextFilter = (s1, s2) => { return true; }; // Display all autocomplete items regardless of what's typed
		Bitrate.KeyDown += Bitrate_KeyDown;
		Bitrate.TextChanged += Bitrate_TextChanged;
		Bitrate.PointerReleased += (o, e) => { e.Handled = true; };

		NameInput.LostFocus += (o, e) => EndEditName();
		NameInput.KeyDown += (o, e) =>
		{
			if (e.Key == Key.Enter) { File.Name = NameInput.Text ?? "";  EndEditName(); e.Handled = true; }
			if (e.Key == Key.Escape) { EndEditName(); e.Handled = true; }
		};

		for (var i = 0; i <= 8; i++)
		{
			NoteInput.Items.Add($"C-{i}");
			NoteInput.Items.Add($"C#{i}");
			NoteInput.Items.Add($"D-{i}");
			NoteInput.Items.Add($"D#{i}");
			NoteInput.Items.Add($"E-{i}");
			NoteInput.Items.Add($"F-{i}");
			NoteInput.Items.Add($"F#{i}");
			NoteInput.Items.Add($"G-{i}");
			NoteInput.Items.Add($"G#{i}");
			NoteInput.Items.Add($"A-{i}");
			NoteInput.Items.Add($"A#{i}");
			NoteInput.Items.Add($"B-{i}");
		}
		NoteInput.SelectionChanged += (o, e) => { AssignedNote.Text = NoteInput.SelectedValue?.ToString(); };
		NoteInput.SelectedValue = "C-4";
		AssignedNote.PointerReleased += (o, e) => { e.Handled = true; SampleInfo.IsVisible = !(NoteInput.IsDropDownOpen = NoteInput.IsVisible = true); };
		NoteInput.DropDownClosed += (o, e) => { SampleInfo.IsVisible = !(NoteInput.IsVisible = false); };
	}

	private void Bitrate_TextChanged(object? sender, TextChangedEventArgs e)
	{
		if (File?.UncompressedSourceFile == null) return;
		if (_cancellationTokenSource != null && _cancellationTokenSource.IsCancellationRequested) _cancellationTokenSource.Cancel();

		Bitrate.Classes.Set("invalid", File.CompressionBitRate < 9 || File.CompressionBitRate > 256);
		if (File.CompressionBitRate < 9 || File.CompressionBitRate > 256) return;

		var sampleRate = File.CompressionBitRate * 16000 / 72; // kbit to BRR samples. 144kbit = 32000hz

		_cancellationTokenSource = new CancellationTokenSource();
		var file = File; // Can't access datacontext property from async thread
		Task.Run(async () =>
		{
			await Task.Delay(400, _cancellationTokenSource.Token);
			file.Resample(sampleRate, _cancellationTokenSource.Token);
			if (_cancellationTokenSource.Token.IsCancellationRequested) return;
			PreviewSampleRate = file.CompressedSampleRate;

		}, _cancellationTokenSource.Token);
	}

	private void Bitrate_KeyDown(object? sender, KeyEventArgs e) {
		if (e.KeySymbol != null && !Regex.IsMatch(e.KeySymbol, @"[0-9]")) e.Handled = true;

		if (Bitrate.IsDropDownOpen) return; // Don't handle up/down keys when dropdown is open (handled by dropdown instead
		if (e.Key == Key.Up) { if (int.TryParse(Bitrate.Text, out var bitRate) && bitRate < 256) Bitrate.Text = (bitRate + 1).ToString(); e.Handled = true; }
		if (e.Key == Key.Down) { if (int.TryParse(Bitrate.Text, out var bitRate) && bitRate > 9) Bitrate.Text = (bitRate - 1).ToString(); e.Handled = true; }
	}

	private Brewsic.Playback.PreviewPlayer? GetPreviewPlayer()
	{
		var topLevel = TopLevel.GetTopLevel(this) as Window;
		var mainWindow = topLevel as MainWindow;
		if (mainWindow == null) mainWindow = topLevel?.Owner as MainWindow;
		return mainWindow?.PreviewPlayer;
	}

	private void EditName(object? sender, PointerReleasedEventArgs e)
	{
		e.Handled = true;
		SampleInfo.IsVisible = false;
		NameInput.Text = File?.Name;
		NameInput.SelectAll();
		NameInput.IsVisible = true;
		NameInput.Focus();
	}
	private void EndEditName()
	{
		NameInput.IsVisible = false;
		SampleInfo.IsVisible = true;
	}
	private void Play()
	{
		var brrSample = File?.BrrSample;
		if (brrSample != null) GetPreviewPlayer()?.PlayBrr(brrSample.SampleData, (int)PreviewSampleRate, brrSample.LoopStart);
	}
	private void Play(object? sender, RoutedEventArgs e)
    {
		PreviewSampleRate = File?.CompressedSampleRate ?? 32000;
		Play();
	}
	private void PlayPitchUp(object? sender, RoutedEventArgs e)
	{
		PreviewSampleRate *= SemitoneFactor;
		if (PreviewSampleRate > 65000) PreviewSampleRate = 8000;
		Play();
	}
	private void PlayPitchDown(object? sender, RoutedEventArgs e)
	{
		PreviewSampleRate /= SemitoneFactor;
		if (PreviewSampleRate < 7900) PreviewSampleRate = 64000;
		Play();
	}

	private void PlayUncompressed(object? sender, RoutedEventArgs e)
	{
		if (File?.UncompressedSourceFile != null)
		{
			GetPreviewPlayer()?.PlayFile(File.UncompressedSourceFile);
			PreviewSampleRate = File.CompressedSampleRate;
		}
	}

	private void Expand(object? sender, PointerReleasedEventArgs e) => File.Expanded = true;

	private void Collapse(object? sender, PointerReleasedEventArgs e) => File.Expanded = false;

}