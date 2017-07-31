# BioFormatsLink for Wolfram Language

![BioFormatsLinkLogo](logo.png)

BioFormatsLink is a package for interacting with [Bio-Formats](http://www.openmicroscopy.org/bio-formats/) library in the [Wolfram Language](https://www.wolfram.com/language/). BioFormatsLink supports 11.2 and later versions of Wolfram Language deployments for the desktop, including [Wolfram Desktop](https://www.wolfram.com/desktop/) and [Mathematica](https://www.wolfram.com/mathematica/).

### Installing the BioFormatsLink release

The BioFormatsLink release comes in the form of a `.paclet` file, which contains the entire package and its documentation. Download the latest release from the [Github repo's releases page](https://github.com/WolframResearch/BioFormatsLink/releases). To install, run the following command in the Wolfram Language:

    PacletInstall["/full/path/to/BioFormatsLink.paclet"]

This will permanently install the BioFormatsLink paclet. The Wolfram Language will always use the latest installed version of BioFormatsLink. Installed versions can be enumerated using the command:

    PacletFind["BioFormatsLink"]

And all versions can be uninstalled using the command:

    PacletUninstall["BioFormatsLink"]