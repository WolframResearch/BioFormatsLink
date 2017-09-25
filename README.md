# BioFormatsLink for Wolfram Language

![BioFormatsLinkLogo](logo.png)

BioFormatsLink is a package for interacting with [Bio-Formats](http://www.openmicroscopy.org/bio-formats/) library in the [Wolfram Language](https://www.wolfram.com/language/). BioFormatsLink supports 10.2 and later versions of Wolfram Language deployments for the desktop, including [Wolfram Desktop](https://www.wolfram.com/desktop/) and [Mathematica](https://www.wolfram.com/mathematica/).

### Building BioFormatsLink
    
To build a `.paclet` file:

* Build documentation using Ant's script in Scripts/docbuild.xml.
* Open Scripts/assemblePaclet.wl in the Wolfram system.
* You may want to modify the value of $versionNumber variable.
* Run the entire package.
* The `.paclet` file will be created in `/full/path/to/BioFormatsLink/build/date-time` directory.

### Installing the BioFormatsLink release

The BioFormatsLink release comes in the form of a `.paclet` file, which contains the entire package and its documentation. Download the latest release from the [Github repo's releases page](https://github.com/WolframResearch/BioFormatsLink/releases). To install, run the following command in the Wolfram Language:

    PacletInstall["/full/path/to/BioFormatsLink-version.paclet"]

This will permanently install the BioFormatsLink paclet. The Wolfram Language will always use the latest installed version of BioFormatsLink. Installed versions can be enumerated using the command:

    PacletFind["BioFormatsLink"]

And all versions can be uninstalled using the command:

    PacletUninstall["BioFormatsLink"]
    
 
### Using BioFormatsLink

To access the documentation, open the notebook interface help viewer, and search for BioFormatsLink/tutorial/ImportingDataFromBioFormats. This will open a tutorial on how to import data using BioFormatsLink.

To start, load the BioFormatLink package, and try opening an image file.

    Needs["BioFormatsLink`"]
    Import["ExampleData/spikey.tiff", "BioFormats"] 
    
### More...

See the following files for more information:

* [LICENSE.txt](LICENSE.txt) - BioFormatsLink license
* [CONTRIBUTING.md](CONTRIBUTING.md) - Guidelines for contributing to BioFormatsLink
