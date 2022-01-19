{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "trace-dispatcher"; version = "1.29.0"; };
      license = "NONE";
      copyright = "2020 IOHK";
      maintainer = "operations@iohk.io";
      author = "Juergen Nicklisch-Franken";
      homepage = "";
      url = "";
      synopsis = "Package for development of simple and efficient tracers\nbased on the arrow based contra-tracer package";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "CHANGELOG.md" "README.md" "doc/trace-dispatcher.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
          (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
          (hsPkgs."ekg-forward" or (errorHandler.buildDepError "ekg-forward"))
          (hsPkgs."hostname" or (errorHandler.buildDepError "hostname"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."trace-forward" or (errorHandler.buildDepError "trace-forward"))
          (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
        modules = [
          "Cardano/Logging"
          "Cardano/Logging/Types"
          "Cardano/Logging/Trace"
          "Cardano/Logging/Configuration"
          "Cardano/Logging/DocuGenerator"
          "Cardano/Logging/Formatter"
          "Cardano/Logging/FrequencyLimiter"
          "Cardano/Logging/Tracer/EKG"
          "Cardano/Logging/Tracer/Standard"
          "Cardano/Logging/Tracer/Forward"
          "Cardano/Logging/Tracer/Composed"
          "Cardano/Logging/Utils"
          "Control/Tracer/Arrow"
          "Control/Tracer"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "trace-dispatcher-examples" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
            (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
            (hsPkgs."hostname" or (errorHandler.buildDepError "hostname"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."trace-dispatcher" or (errorHandler.buildDepError "trace-dispatcher"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          buildable = true;
          modules = [
            "Examples/TestObjects"
            "Examples/Aggregation"
            "Examples/Trivial"
            "Examples/Routing"
            "Examples/EKG"
            "Examples/Configuration"
            "Examples/FrequencyLimiting"
            "Examples/Documentation"
            ];
          hsSourceDirs = [ "examples" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "trace-dispatcher-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
            (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
            (hsPkgs."hostname" or (errorHandler.buildDepError "hostname"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."trace-dispatcher" or (errorHandler.buildDepError "trace-dispatcher"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          modules = [
            "Cardano/Logging/Test/Types"
            "Cardano/Logging/Test/Oracles"
            "Cardano/Logging/Test/Config"
            "Cardano/Logging/Test/Tracer"
            "Cardano/Logging/Test/Messages"
            "Cardano/Logging/Test/Script"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "trace-dispatcher-test.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "13";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "13";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/trace-dispatcher; echo source root reset to \$sourceRoot";
    }