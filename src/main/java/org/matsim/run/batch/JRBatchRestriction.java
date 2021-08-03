package org.matsim.run.batch;

import org.matsim.core.config.Config;
import org.matsim.episim.BatchRun;
import org.matsim.run.RunParallel;
import org.matsim.run.modules.SnzBerlinProductionScenario;

import javax.annotation.Nullable;

import static org.matsim.episim.EpisimConfigGroup.ActivityHandling;

public class JRBatchRestriction implements BatchRun<JRBatchRestriction.Params> {

	@Override
	public SnzBerlinProductionScenario getBindings(int id, @Nullable Params params) {
		return new SnzBerlinProductionScenario.Builder()
				.setSnapshot(SnzBerlinProductionScenario.Snapshot.no)
				.setActivityHandling(ActivityHandling.startOfDay)
				.setRestrictBerlinMitteOctober2020(params != null ? params.restrictBerlinMitteOctober2020 : SnzBerlinProductionScenario.RestrictBerlinMitteOctober2020.no) //params != null ? params.restrictBerlinMitteOctober2020 : SnzBerlinProductionScenario.RestrictBerlinMitteOctober2020.no)
				.setLocationBasedRestrictions(params != null ? params.locationBasedRestrictions : SnzBerlinProductionScenario.LocationBasedRestrictions.no)
				.setSample(1)
				.setLocationBasedContactIntensity(SnzBerlinProductionScenario.LocationBasedContactIntensity.no)
				.createSnzBerlinProductionScenario();
	}

	@Override
	public Metadata getMetadata() {
		return Metadata.of("berlin", "locationBasedRestrictions");
	}

	@Override
	public Config prepareConfig(int id, Params params) {

		SnzBerlinProductionScenario module = getBindings(id, params);


		assert module != null;
		Config config = module.config();

		config.global().setRandomSeed(params.seed);

		//		EpisimConfigGroup episimConfig = ConfigUtils.addOrGetModule(config, EpisimConfigGroup.class);

		//		episimConfig.setActivityHandling(params.activityHandling);

		return config;
	}

	public static final class Params {

		@EnumParameter(SnzBerlinProductionScenario.LocationBasedRestrictions.class)
		SnzBerlinProductionScenario.LocationBasedRestrictions locationBasedRestrictions;

		@GenerateSeeds(1)
		public long seed;

		@EnumParameter(SnzBerlinProductionScenario.RestrictBerlinMitteOctober2020.class)
		SnzBerlinProductionScenario.RestrictBerlinMitteOctober2020 restrictBerlinMitteOctober2020;

//		@EnumParameter(ActivityHandling.class)
//		ActivityHandling activityHandling;

	}

	public static void main(String[] args) {
		String[] args2 = {
				RunParallel.OPTION_SETUP, JRBatchRestriction.class.getName(),
				RunParallel.OPTION_PARAMS, Params.class.getName(),
				RunParallel.OPTION_ITERATIONS, Integer.toString(450),
				RunParallel.OPTION_METADATA
		};

		RunParallel.main(args2);
	}


}

