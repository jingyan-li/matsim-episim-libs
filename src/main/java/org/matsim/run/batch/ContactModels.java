package org.matsim.run.batch;

import com.google.inject.AbstractModule;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.episim.BatchRun;
import org.matsim.episim.EpisimConfigGroup;
import org.matsim.episim.model.*;
import org.matsim.episim.policy.FixedPolicy;
import org.matsim.run.modules.SnzBerlinScenario25pct2020;
import org.matsim.run.modules.SnzBerlinWeekScenario2020;

import javax.annotation.Nullable;
import java.util.Map;


/**
 * Runs comparing different contact models for berlin.
 */
public class ContactModels implements BatchRun<ContactModels.Params> {


	@Override
	public AbstractModule getBindings(int id, @Nullable Params params) {
		// for base case
		if (params == null)
			return new SnzBerlinWeekScenario2020();

		return new SnzBerlinWeekScenario2020(25, false,
				params.contactModel != DefaultContactModel.class, params.contactModel);
	}

	@Override
	public Metadata getMetadata() {
		return Metadata.of("berlin", "contactModels");
	}

	@Override
	public Config prepareConfig(int id, Params params) {

		SnzBerlinWeekScenario2020 module = new SnzBerlinWeekScenario2020();
		Config config = module.config();

		EpisimConfigGroup episimConfig = ConfigUtils.addOrGetModule(config, EpisimConfigGroup.class);

		// TODO

		// TODO: run1 OldSymModel
		// run2 (new) Symmetric / n=20
		// run3 default
		// run4 pairwise

		double param = Double.NaN;
		if (params.contactModel == OldSymmetricContactModel.class) {
			param = 1.07e-5;
		} else if (params.contactModel == SymmetricContactModel.class) {
			param = 2.53e-5;
		} else if (params.contactModel == DefaultContactModel.class) {
			param = 1.45e-5;
		} else if (params.contactModel == PairWiseContactModel.class) {

		}

		episimConfig.setCalibrationParameter(param);

		if (params.unrestricted.equals("yes")) {
			episimConfig.setPolicy(FixedPolicy.class, FixedPolicy.config().build());
		} else {

			SnzBerlinScenario25pct2020.BasePolicyBuilder basePolicyBuilder = new SnzBerlinScenario25pct2020.BasePolicyBuilder(episimConfig);
			basePolicyBuilder.setCiCorrections(Map.of("2020-03-07", 0.32));

			// TODO: ci corrections

			episimConfig.setPolicy(FixedPolicy.class, basePolicyBuilder.build().build());

		}

		config.global().setRandomSeed(params.seed);

		return config;
	}

	public static final class Params {

		@GenerateSeeds(50)
		public long seed;

		@ClassParameter({DefaultContactModel.class, OldSymmetricContactModel.class,
				SymmetricContactModel.class, PairWiseContactModel.class})
		public Class<? extends ContactModel> contactModel;

		@StringParameter({"yes", "no"})
		public String unrestricted;

	}

}