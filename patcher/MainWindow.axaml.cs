using Avalonia.Controls;
using Avalonia.Interactivity;
using Avalonia.Platform.Storage;
using System.Collections.Generic;
using System.IO;

namespace Patcher;
public partial class MainWindow : Window
{
	public MainWindow()
	{
		InitializeComponent();
	}

	public async void PickRomImage(object sender, RoutedEventArgs args)
	{
		var files = await StorageProvider.OpenFilePickerAsync(new FilePickerOpenOptions
		{
			Title = "Find tracker ROM",
			AllowMultiple = false
		});

		if (files.Count >= 1)
		{
			// Open reading stream from the first file.
			await using var stream = await files[0].OpenReadAsync();
			using var streamReader = new StreamReader(stream);
			// Reads all the content of file as a text.
			var fileContent = await streamReader.ReadToEndAsync();
		}
	}
}